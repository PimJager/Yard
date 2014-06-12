module Plugin.Yard.ExprStack where

import Prelude hiding (id, rem)

import GhcPlugins
import TypeRep

import Data.List

import Plugin.Yard.Stack

-- | Different expression types found in Core
data ExprFrame 	= EUnkown (Expr CoreBndr)
				| ELiteral Literal
				| EVariable CoreBndr Int
				| ELambda ExprFrame ExprStack
				| EType Type

type ExprStack = Stack ExprFrame

showExprFrame :: ExprFrame -> CoreM String
showExprFrame ef = getDynFlags >>= \fl -> case ef of 
	(EUnkown e) -> return $ "Unknown: " ++ (showSDoc fl $ ppr e)
	(ELiteral l) -> return $ "Lit: " ++ (showSDoc fl $ ppr l)
	(EVariable v a) -> return $ "Var: " ++ (showSDoc fl $ ppr v) ++ "(" ++ show a ++ ")"
	(ELambda l e) -> showExprFrame l >>= \binder ->
					 mapM showExprFrame e >>= \inner -> 
						return $ "Lam: \\" ++ binder ++ " -> [" ++ intercalate "\n\t" inner ++ " ] "
	(EType _) -> return "" -- $ "Type: " ++ (showSDoc fl $ ppr t)


-- | Converts the Expr b tree into an ExprStack 
-- 	 which represents the same Expression, but as a stack
exprStack :: Expr CoreBndr -> ExprStack
exprStack e = go e []
	where
		go :: Expr CoreBndr -> ExprStack -> ExprStack
		go (Lit l) s 			= push (ELiteral l) s
		go (Var id) s 			= push (EVariable id arr) s
			where arr = arity $ varType id
		go (App expr arg) s 	= go expr $ go arg s
		go (Lam i expr) s		= push (ELambda (EVariable i (0)) (go expr [])) s
		go (Type t) s			= push (EType t) s
		go expr s				= push (EUnkown expr) s


-- testen voor dingen als:
-- Parser Cont
-- type Cont a b = a -> b
-- en newtype Cont = C (a -> b)
-- anders het Type (ipv repType) en die tellen.
-- dan eventueel ook naar beneden doorgeven, zodat als we dit hebben:
-- x :: a -> a -> (a->a)
-- x a = \b -> a + b
-- dat we dan weten dat de ariteit van x 1 is, en het resultaat ook ariteit 1 heeft
-- en x dus niet ariteit 2 heeft.
arity :: Type -> Int
arity ty = case flattenRepType $ repType ty of
	[ty'] -> go ty'
	_ -> error "arity"
	where
		go (FunTy _ t) = 1 + go t
		go _ = 0
