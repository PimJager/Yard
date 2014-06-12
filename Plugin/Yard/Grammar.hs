module Plugin.Yard.Grammar(
	Grammar(..),

	yardStackToGrammar
	) where

import Prelude hiding (id, rem)

import GhcPlugins

import Data.List

import Plugin.Yard.YardStack

data Grammar	= GReturn Grammar -- ^ return a
				| GSequence Grammar Grammar -- ^ a >> b
				| GBind Grammar Grammar -- ^ a >>= b
				| GChoice Grammar Grammar -- ^ a <|> b 
				| GMany Grammar -- ^ many a
				| GOptional Grammar Grammar -- ^ b <|> (return a)
				| GString Grammar -- ^ "a"
				| GChar Grammar -- ^ 'a'
				| GAlpha -- ^ α
				| GDigit -- ^ [0..9]
				| GNumber -- ^ many digit
				| GDontCare -- ^ .
				| GLambda Grammar Grammar -- ^ \a -> b
				| GEmpty -- ^ []
				| GNonTerminal String [Grammar] -- ^ locally defined Parser
				| GLiteral  String -- ^ a literal, used for tree completion
				| GVariable String [Grammar] -- ^ a function with possible arguments
				| GUnpack String Grammar -- ^ an GHC.Types function
				| GUnknown (Expr CoreBndr) -- ^ an unknown construct (case, let, coercion, etc.)
				| GType	(Expr CoreBndr) Grammar -- ^ a type construct (Type t)
				| GPanic String Grammar

-- | returns the grammar described by a stack
yardStackToGrammar :: YardStack -> Grammar
yardStackToGrammar = (fst . processVarStack)
		-- the top APP holds the entire stack. It's not possible to have anything on the stack after processing the
		-- the top app frame, as the entire stack should only be one expression
	where
		processVarStack :: YardStack -> (Grammar, YardStack)
		processVarStack (x:xs) = case x of
				VReturn 			-> (GReturn g1, xs')
				VBind 				-> (GBind g1 g2, xs'')
				VSequence 			-> (GSequence g1 g2, xs'')
				VChoice 			-> (GChoice g1 g2, xs'')
				VMany 				-> (GMany g1, xs')
				VOptional 			-> (GOptional g1 g2, xs'')
				VZero 				-> (GEmpty, xs)
				VItem 				-> (GDontCare, xs)
				VSatisfy 			-> processVarStack xs
				VString 			-> (GString g1, xs')
				VChar 				-> (GChar g1, xs')
				VAlpha 				-> (GAlpha, xs)
				VDigit 				-> (GDigit, xs)
				VNumber 			-> (GNumber, xs)
				VLocal f ar 		-> let (gs, rem) = readArgs ar xs in (GNonTerminal f gs, rem)
				l@(VLiteral _) 		-> let (str, rem) = readLiteral (l:xs) in (GLiteral str, rem)
				v@(VVariable _ _) 	-> readVariable (v:xs)
				VLambda b ys 		-> let (binder, _) = readVariable [b] in (GLambda binder $ yardStackToGrammar ys, xs)
				VUnpack s 			-> (GUnpack s g1, xs')
				VType s 			-> (GType s g1, xs')
				VUnknown s 			-> (GUnknown s, xs)
			where
				(g1, xs') 	= processVarStack xs
				(g2, xs'') 	= processVarStack xs'
		processVarStack [] = (GEmpty, [])

		readVariable :: YardStack -> (Grammar, YardStack)
		readVariable [] 		= (GPanic "readVar empty stack" GEmpty, [])
		readVariable (VVariable l n:xs) = let (gs, rem) = readArgs n xs in (GVariable l gs, rem)
		readVariable (_:xs) 	= readVariable xs

		readArgs :: Int -> YardStack -> ([Grammar], YardStack)
		readArgs 0 xs = ([], xs)
		readArgs n xs = let (l, rem) = processVarStack xs in let (res, rem') = readArgs (n-1) rem in (l:res, rem')

		readLiteral :: YardStack -> (String, YardStack)
		readLiteral (VLiteral l:xs) = (l, xs)
		readLiteral (_:xs) = readLiteral xs

instance Show Grammar where
	show (GReturn ret) = concat ["return[", show ret, "]"]
	show (GSequence left right) = concat ["(", show left, ", ", show right, ")"]
	show (GBind left right) = concat ["(", show left, " => ", show right, ")"]
	show (GChoice left right) = "(" ++ show left ++ ") | (" ++ show right ++ ")"
	show (GMany g) = "(" ++ show g ++ ")*"
	show (GOptional def g) = concat ["(", show g, " | ", show def, ")"]
	show (GString str) = concat ["Str(", show str, ")"]
	show (GChar ch) = concat ["Ch(", show ch, ")"]
	show (GAlpha) = "alpha"
	show (GDigit) = "[0..9]"
	show (GNumber) = "[0..9]+"
	show (GDontCare) = "."
	show (GLambda b ys) = concat ["<\\", show b, " -> ", show ys, ">"]
	show (GEmpty) = "&"
	show (GPanic s g) = concat ["!PANIC: ", s, "! ", show g]
	show (GNonTerminal e gs) = concat $ e : if length gs > 0 then (["["] ++ (intersperse " " $ map show gs) ++ ["]"]) else []
	show (GVariable s gs) = concat $ s : if length gs > 0 then (["["] ++ (intersperse " " $ map show gs) ++ ["]"]) else []
	show (GLiteral l) = l
	show (GUnpack _ gs) = concat ["^", show gs]
	show (GUnknown _) = "???"
	show (GType _ gs) = concat ["#(", show gs, ")"]