module Plugin.Yard.YardStack(
	YardStack, YardThing(..),

	frameToYT, showYSS, coreMShowYT, coreMShowYSS
	) where

import Prelude hiding (id, rem)

import GhcPlugins

import Data.Char hiding (chr)
import Data.List
import Control.Applicative

import Plugin.Yard.Stack
import Plugin.Yard.ExprStack

-- | YardThing represents things found in an ExprFrame that we understand
--	 Which bassicly means all functions defined in Text.Yard and parser non-terminals
--   from the module we are currently optimising. All other constructs, shuch as unknown
--   variables are packed. 
data YardThing 	= VReturn -- ^ monadic return, 1 argument
				| VBind -- ^ monadic bind (>>=), 2 arguments
				| VSequence -- ^ monadic sequence (>>), 2 arguments
				| VChoice -- ^ alternative choice (<|>), 2 arguments
				| VMany -- ^ library many, 1 argument
				| VOptional -- ^ library optinal, 1 literal argument, 1 argument
				| VZero -- ^ zero Parser, terminal
				| VItem -- ^ item Parser, terminal
				| VSatisfy -- ^ satisy Parser, 1 argument
				| VString -- ^ string Parser, 1 literal argument
				| VChar -- ^ char Parser, 1 literal argument
				| VAlpha -- ^ alpha Parser, terminal
				| VDigit -- ^ digit Parser, terminal
				| VNumber -- ^ number Parser, terminal
				| VLocal String Int -- ^ a Parser in this module, with its arity
				| VLiteral String -- ^ a Literal
				| VVariable String Int -- ^ a Variable, has Int arguments
				| VLambda YardThing YardStack -- ^ A lambda expression, terminal
				| VUnpack String -- ^ a GHC.Types datatype
				| VType (Expr CoreBndr) -- ^ a type variable, step over
				| VUnknown (Expr CoreBndr) -- ^ an unknown construct (let, case, type, coercion, etc.), step over

type YardStack = Stack YardThing

-- | Expands/unpacks an ExprFrame into a YardThing
--  FUTURE WORK: don't do this using string comparison
-- 	GHC changes the string representation of the function depending
-- 	on whatever it feels like (phase of the moon probably), so there are
-- 	multiple cases here for the same function, most of these come with "works
-- 	on my machine"-level guarantees
frameToYT :: [String] -> ExprFrame -> CoreM YardThing
frameToYT parsers (EVariable bndr arr) = getDynFlags >>= \fl -> return $ go $ showSDoc fl $ ppr bndr
	where
		go :: String -> YardThing
		go s 
			| "Text.Yard.parserReturn" == s 	= VReturn
			| "Text.Yard.parserBind" == s 		= VBind
			| "Text.Yard.parserSequence" == s 	= VSequence
			| "Text.Yard.parserAlternative" == s= VChoice
			| "Text.Yard.parserMany" == s 		= VMany
			| "Text.Yard.optional" == s 		= VOptional
			| "Text.Yard.zero" == s 			= VZero
			| "Text.Yard.item" == s 			= VItem
			| "Text.Yard.satisfy" == s 			= VSatisfy
			| "Text.Yard.string" == s 			= VString
			| "Text.Yard.char" == s 			= VChar
			| "Text.Yard.alpha" == s 			= VAlpha
			| "Text.Yard.digit" == s 			= VDigit
			| "Text.Yard.number" == s 			= VNumber
			| "$f" `isInfixOf` s 				= VType (Var bndr)
			| "GHC.Types.:" `isInfixOf` s 		= VVariable s 2
			| "GHC.Types" `isInfixOf` s 		= VUnpack s 
			| "GHC.CString" `isInfixOf` s 		= VUnpack s
			| any (`isInfixOf` s) parsers 		= VLocal s arr
			| otherwise							= VVariable s arr
frameToYT _ l@(ELiteral _) 		= VLiteral <$> literalToString l
frameToYT p (ELambda i es) 		= frameToYT p i >>= \binder ->
								  mapM (frameToYT p) es >>= \inner ->
									return $ VLambda binder inner
frameToYT _ (EType t)			= return $ VType (Type t)
frameToYT _ (EUnkown u)			= return $ VUnknown u

literalToString :: ExprFrame -> CoreM String
literalToString (ELiteral bndr) = getDynFlags >>= \fl -> return $ filter isPrint $ showSDoc fl $ ppr bndr

-- Show instances
instance Show YardThing where
	show VReturn = "return1"
	show VBind = ">>=2"
	show VSequence = ">>2"
	show VChoice = "<|>2"
	show VMany = "*1"
	show VOptional = "opt2"
	show VZero = "zero0"
	show VItem = "item0"
	show VSatisfy = "satisfy1"
	show VString = "string1"
	show VChar = "char1"
	show VAlpha = "alpha0"
	show VDigit = "digit0"
	show VNumber = "number0"
	show (VLocal s a) = concat ["loc:", s, show a]
	show (VLiteral s) = concat ["lit:", s]
	show (VVariable s i) = concat ["var:", s, show i]
	show (VLambda bndr st) = concat ["\\", show bndr, "->["] ++ showYSS st ++ "]"
	show (VUnpack s) = concat ["unp:", s, "1"]
	show (VType _) = "&"
	show (VUnknown _) = "???"

showYSS :: YardStack -> String
showYSS = concat . intersperse "\r\n" . map show

coreMShowYSS :: YardStack -> CoreM String
coreMShowYSS = fmap (concat . intersperse "\r\n") . mapM coreMShowYT

coreMShowYT :: YardThing -> CoreM String
coreMShowYT (VUnknown d) = getDynFlags >>= \fl -> return $ showSDoc fl $ ppr d
coreMShowYT (VLambda bndr st) = coreMShowYT bndr >>= 
	\bndr' -> coreMShowYSS st >>= 
	\st' -> return $ concat ["\\", bndr', "->["] ++ st' ++ "]"
coreMShowYT d 	= return $ show d