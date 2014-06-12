{- |
	This module contains functions which transform a Grammar into a StateMachine

	Statemachines are represented as a list of transitions and a map which holds the lambda
	bindings. All transitions produce a result and some also consume input (Consumer).
	Consumers produce the consumed input as result, produces produce whatever the function
	result is. The produced result is bound to the end state of the transition. Epsilon
	transitions simply produce the result from their start state and consume no input.
-}
module Plugin.Yard.StateMachine (
		StateMachine(..), Label, Transition(..), Consumer(..), Producer(..),

		initial, grammarToSM
	) where

import Prelude hiding (id, rem)

import GhcPlugins hiding (Var)

import Control.Monad.State
import Data.List
import Control.Applicative
import Data.Maybe
import Data.Monoid hiding (Any)
import qualified Data.Map.Strict as Map

import Plugin.Yard.Grammar


data StateMachine = SM [(Label, Transition, Label)] (Map.Map String Label)

type Label = Int
data Transition = C Consumer
				| P Producer 
				| Epsilon
data Consumer 	= Alpha | CH Char | Str String | Digit | Number | Any | NonTerminal String [StateMachine] 
data Producer 	= Var String [StateMachine]
				| Unknown (Expr CoreBndr)
				| Literal String

data ST = ST {counter :: Label}

instance Monoid StateMachine where
	mempty = SM [] Map.empty
	mappend (SM s1 m1) (SM s2 m2) = SM (s1++s2) (Map.union m2 m1) 
							-- union favors the first argument when keys overlap (keys should not overlap)

initial :: ST 
initial = ST 1

{- |
	Cleans the grammar of unwanted constructs:
		- GReturn
		- GUnpack
		- GType
		- GPanic
-}
cleanse :: Grammar -> Grammar
cleanse k = case k of
	GReturn g 	-> cleanse g
	GUnpack _ g -> cleanse g
	GType _ g 	-> cleanse g
	GPanic _ g 	-> cleanse g
	GSequence g1 g2 -> GSequence (cleanse g1) (cleanse g2)
	GBind g1 g2 -> GBind (cleanse g1) (cleanse g2)
	GChoice g1 g2 -> GChoice (cleanse g1) (cleanse g2)
	GMany g -> GMany $ cleanse g
	GOptional g1 g2 -> GOptional (cleanse g1) (cleanse g2)
	GString g -> GString $ cleanse g
	GChar g -> GChar $ cleanse g
	GLambda g1 g2 -> GLambda (cleanse g1) (cleanse g2)
	GNonTerminal s gs -> GNonTerminal s $ map cleanse gs
	GVariable s gs -> GVariable s $ map cleanse gs
	g -> g

-- | transforms a grammar to a StateMachine, when a start label is
-- 	 given that will be used as the label in which the transition starts
-- 	 the end label will always be the next one taken from state.
grammarToSM :: Maybe Label -> Grammar -> State ST StateMachine
grammarToSM start k = do 
	next <- (+1) <$> gets counter 
	let cur = fromMaybe (next - 1) start
	modify (\st -> st{counter = next}) 
	case cleanse k of 
		GSequence g1 g2 -> do
			sm1 <- grammarToSM Nothing g1
			sm2 <- grammarToSM Nothing g2
			return $ buildE cur next `mappend` sm1 `mappend` sm2
		GBind g1 g2 -> do 
			sm <- grammarToSM Nothing $ GSequence g1 g2
			return $ buildE cur next `mappend` sm
		GChoice g1 g2 -> do
			sm1 <- grammarToSM Nothing g1
			l1 <- gets counter
			sm2 <- grammarToSM (Just cur) g2
			l2 <- gets counter
			let next' = l2 + 1
			modify (\st -> st{counter = next'}) 
			return $ buildE cur next `mappend` sm1 `mappend` sm2 `mappend` buildE l2 l1 `mappend` buildE l1 next'
		GMany g -> do
			sm <- grammarToSM Nothing g
			l <- gets counter
			return $ buildE cur next `mappend` sm `mappend` buildE l next
		GOptional _ _ -> error "optional not implemented in statemachine"
		GString (GVariable "GHC.Base.build" [GLambda _ (GLiteral s)]) -> return $ buildC cur (Str s) next
		GString t -> error ("string tree malformed: " ++ show t)
		GChar (GLiteral s) -> return $ buildC cur (CH $ s !! 1) next
		GChar t -> error ("char tree malformed: " ++ show t)
		GAlpha -> return $ buildC cur Alpha next
		GDigit -> return $ buildC cur Digit next
		GNumber -> return $ buildC cur Number next
		GDontCare -> return $ buildC cur Any next
		GLambda (GVariable s _) g -> do
			sm <- grammarToSM Nothing g
			return $ buildE cur next `mappend` (buildM [(s, next)]) `mappend` sm
		GEmpty -> return $ buildE cur next
		GNonTerminal s gs -> do
			let sms = map (\g -> evalState (grammarToSM Nothing g) initial) gs
			return $ buildC cur (NonTerminal s sms) next
		GLiteral s -> return $ buildP cur (Literal s) next
		GVariable s gs -> do
			let sms = map (\g -> evalState (grammarToSM Nothing g) initial) gs
			return $ buildP cur (Var s sms) next
		GUnknown expr -> return $ buildP cur (Unknown expr) next
	where 
		buildC c t n = SM [(c, C t, n)] Map.empty
		buildP c t n = SM [(c, P t, n)] Map.empty
		buildE c n   = SM [(c, Epsilon, n)] Map.empty
		buildM ls 	 = SM [] $ Map.fromList ls

-- Show instances
instance Show StateMachine where
	show (SM s m) = (concat $ intersperse "\r\n" $ map show s) ++ show m 

instance Show Transition where
	show t = case t of
		(C c) -> show c
		(P p) -> show p
		(Epsilon) -> "Epsilon"

instance Show Consumer where
	show c = case c of
		Alpha -> "alpha"
		CH chr -> [chr]
		Str s -> s
		Digit -> "[0..9]"
		Number -> "[0..9]*"
		Any -> "*"
		NonTerminal s sms -> concat ([s, "["] ++ (intersperse "," $ map show sms)) ++ "]"

instance Show Producer where
	show p = case p of
		(Var s sms) -> concat (["V:", s, "["] ++ (intersperse "," $ map show sms)) ++ "]"
		(Unknown _) -> "???"
		(Literal s) -> concat ["L:", s]

