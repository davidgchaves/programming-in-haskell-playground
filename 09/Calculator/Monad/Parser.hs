module Calculator.Monad.Parser
(
-- The Parser type (Monad and MonadPlus)
  Parser(..)

-- Basic parsers
, failure
, item
, parse

-- Choice
, (+++)

-- Derived primitives
, sat
, digit
, char
, string
, many
, many1
, nat
, space

-- Ignoring spacing
, token
, natural
, symbol
) where

import Data.Char
import Control.Monad

infixr 5 +++

-- -----------------------------------------------------------------------------
-- Monad and MonadPlus Parser instances

newtype Parser a = P (String -> [(a,String)])

instance Monad Parser where
    return v = P (\inp -> [(v,inp)])
    p >>= f  = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> parse (f v) out)

instance MonadPlus Parser where
    mzero       = P (\inp -> [])
    p `mplus` q = P (\inp -> case parse p inp of
                                []        -> parse q inp
                                [(v,out)] -> [(v,out)])

-- -----------------------------------------------------------------------------
-- Basic parsers

failure :: Parser a
failure = mzero

item :: Parser Char
item = P (\inp -> case inp of
                    []     -> []
                    (x:xs) -> [(x,xs)])

parse           :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

-- -----------------------------------------------------------------------------
-- Choice

(+++)   :: Parser a -> Parser a -> Parser a
p +++ q = p `mplus` q

-- -----------------------------------------------------------------------------
-- Derived primitives

sat   :: (Char -> Bool) -> Parser Char
sat p = item >>= \x ->
        if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

char   :: Char -> Parser Char
char x = sat (== x)

string        :: String -> Parser String
string []     = return []
string (x:xs) = char x >> string xs >> return (x:xs)

many   :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1   :: Parser a -> Parser [a]
many1 p = p >>= \v ->
          many p >>= \vs ->
          return (v:vs)

nat :: Parser Int
nat = many1 digit >>= \xs ->
      return (read xs)

space :: Parser ()
space = many (sat isSpace) >> return ()


-- -----------------------------------------------------------------------------
-- Ignoring spacing

token   :: Parser a -> Parser a
token p = space >>
          p >>= \v ->
          space >>
          return v

natural :: Parser Int
natural = token nat

symbol    :: String -> Parser String
symbol xs = token (string xs)
