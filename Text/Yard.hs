{-# LANGUAGE ExistentialQuantification #-}
{-
	Mini Parsec-like Monadic Parser Combinator library. Used as a research tool for
	a paper on GHC plugins. This is a small and simple version of the monadic parser 
	combinator ideas introduced by Hutton and Meijer. 
	Parsers are by default LL(*) and can not be non-deterministic (unlike the 
	ones in the paper). 
	Parsers can either succeed by completely parsing (Just result) or fail (Nothing)

	@article{hutton1996monadic,
	  title={Monadic parser combinators},
	  author={Hutton, Graham and Meijer, Erik},
	  year={1996},
	  publisher={School of Computer Science and IT}
	}

	Main source for parsec: http://hackage.haskell.org/package/parsec-3.1.5/src/Text/Parsec/Prim.hs 
-}


module Text.Yard (
	Parser(), runParser,

	item, zero, optional, many, char, alpha, digit, string, number
	) where
	-- YARD: Yet Another Research DSL

import Data.Char
import qualified Control.Applicative as Appl
import Control.Monad

import Plugin.Yard.Annotations

{-# ANN module DontOptimize #-}

{- 
	GHC and the inliner like to change the ppr names of the Monad functions depending
	on context and flags and whatever, which is rather annoying (because then our string comparison hack 
	doesn't work) so we explicitly inline those to known functions whose names don't change
-}
{-# NOINLINE parserReturn #-}
parserReturn :: a -> Parser a
parserReturn = \res -> Parser $ \inp -> Just (res, inp)
{-# NOINLINE parserBind #-}
parserBind :: Parser a -> (a -> Parser b) ->  Parser b
parserBind = \p f -> Parser $ maybe Nothing (\(res, rest) -> runParser (f res) rest) . runParser p
{-# NOINLINE parserSequence #-}
parserSequence :: Parser a -> Parser b -> Parser b
parserSequence = \p f -> Parser $ maybe Nothing (\(_, rest) -> runParser f rest) . runParser p
{-# NOINLINE parserAlternative  #-}
parserAlternative :: Parser a -> Parser a -> Parser a
parserAlternative = \p q -> Parser $ \inp -> runParser p inp Appl.<|> runParser q inp
{-# NOINLINE parserMany #-}
parserMany :: Parser a -> Parser [a]
parserMany = Appl.many

-- newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
-- newtype causes a lot of casts in the Core, so for our convenience we use data
data Parser a = Parser (String -> Maybe (a, String))

runParser :: Parser a -> String -> Maybe (a, String)
runParser (Parser f) = f
instance Monad Parser where
	{-# INLINE return #-}
	return 	= parserReturn
	{-# INLINE (>>=) #-}
	(>>=) 	=  parserBind
	{-# INLINE (>>) #-}
	(>>)	= parserSequence

instance Functor Parser where
	{-# INLINE fmap #-}
	fmap 	= liftM 
	{-# INLINE (<$) #-}
	(<$)	= liftM . const

instance Appl.Applicative Parser where
	{-# INLINE pure #-}
	pure 	= return
	{-# INLINE (<*>) #-}
	(<*>) 	= ap
	{-# INLINE (*>) #-}
	(*>)	= (>>)
	{-# INLINE (<*) #-}
	a <* b	= a >>= \r -> b >> return r

instance Appl.Alternative Parser where
	{-# INLINE empty #-}
	empty = zero
	{-# INLINE (<|>) #-}
	(<|>) = parserAlternative

-- | matches any character
{-# NOINLINE item #-}
item :: Parser Char
item = Parser $ \inp -> case inp of
	[] -> Nothing
	(x:xs) -> Just (x,xs)

-- | always fails
{-# NOINLINE zero #-}
zero :: Parser a
zero = Parser $ const Nothing

-- | matches any character which satisfies f
{-# NOINLINE satisfy #-}
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = item >>= (\r -> if f r then return r else zero)

-- | Trys a parser, when it fails returns default
{-# NOINLINE optional #-}
optional :: a -> Parser a -> Parser a
optional def p = p `parserAlternative` return def

-- | zero or more
{-# INLINE many #-}
many :: Parser a -> Parser [a]
many = parserMany

-- | matches the given char 
{-# NOINLINE char #-}
char :: Char -> Parser Char
char c = satisfy (== c)

-- | matches any unicode letter
{-# NOINLINE alpha #-}
alpha :: Parser Char
alpha = satisfy isAlpha

-- | matches any digit
{-# NOINLINE digit #-}
digit :: Parser Char
digit = satisfy isDigit

-- | matches a given string
{-# NOINLINE string #-}
string :: String -> Parser String
string s = mapM_ char s >> return s

-- | reads a number (one or more digits) from input
{-# NOINLINE number #-}
number :: Parser String
number = digit >>= \x -> many digit >>= \xs -> return (x:xs)