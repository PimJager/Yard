module Plugin.Yard.Pass where

import GhcPlugins

import Data.List
import Control.Monad.State
import Control.Applicative

import Plugin.Yard.ExprStack
import Plugin.Yard.YardStack
import Plugin.Yard.Grammar
import qualified Plugin.Yard.StateMachine as State

-- | passes over the binds in the module and prints the grammars and state machines.
pass :: ModGuts -> CoreM ModGuts
pass guts = getParsers (mg_binds guts) >>= \parsers -> bindsOnlyPass (mapM $ printParser parsers) guts
	where
		printParser :: [String] -> CoreBind -> CoreM CoreBind
		printParser parsers bnd = do
			case bnd of
				(NonRec b e) -> printBndr parsers b e
				(Rec lst) -> mapM_ (\(b,e) -> printBndr parsers b e) lst
			return bnd
		printBndr :: [String] -> CoreBndr -> Expr CoreBndr -> CoreM ()
		printBndr parsers b e = do
			fl <- getDynFlags
			isPars <- isParser b
			rhs <- yardStackToGrammar <$> mapM (frameToYT parsers) (exprStack e)
			let sm = show $ evalState (State.grammarToSM Nothing rhs) State.initial
			when isPars $ putMsgS $ showSDoc fl (ppr b) ++ ": " ++ show rhs ++ "\r\n" ++ sm

-- | a list of all parser-(non-)terminals in the supplied CoreProgram. The list
-- 	 contains all names (lhs) of the parser functions.
getParsers :: CoreProgram -> CoreM [String]
getParsers bnds = evalStateT (bndrStr (accBndr bnds)) []
	where
		accBndr = foldl' accBndr' [] 
		accBndr' :: [CoreBndr] -> CoreBind -> [CoreBndr]
		accBndr' acc (NonRec b _) = b:acc
		accBndr' acc (Rec bs) = map fst bs ++ acc

		bndrStr :: [CoreBndr] -> StateT [String] CoreM [String]
		bndrStr (b:bndrs) = do
			str <- (lift $ printCoreBndr b)
			ss <- get 
			toPut <- lift $ isParser b
			when toPut $ put (str:ss)
			bndrStr bndrs
		bndrStr [] = get

		printCoreBndr :: CoreBndr -> CoreM String
		printCoreBndr bndr = getDynFlags >>= \fl -> return $ showSDoc fl $ ppr bndr

-- | Check if this parser is a binder by checking of the ppr version of its type
-- 	 Might not work when the parser is hidden in a manod stack or whatever, not tested.
isParser :: CoreBndr -> CoreM Bool
isParser b = getDynFlags >>= \fl -> return $ isInfixOf "Text.Yard.Parser" $ showSDoc fl $ ppr $ varType b

