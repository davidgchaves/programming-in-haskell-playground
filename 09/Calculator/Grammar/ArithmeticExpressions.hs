module Calculator.Grammar.ArithmeticExpressions
(
  expr
, term
, factor
) where

import Calculator.Monad.Parser

-- 8.8 Aritmethic expressions (EXPANDED for - and /)
--  A Grammar for a simple form of arithmetic expressions:
--      - built up from natural numbers
--      - using addition, substraction, multiplication, division and parentheses
--      - addition, substraction, multiplication and division associate to the right
--      - multiplication and division have higher priority than addition and substraction
--  Examples:
--      - 2 + 3 + 4 means 2 + (3 + 4)
--      - 2 * 3 + 4 means (2 * 3) + 4
--  Final Grammar:
--      expr   ::= term (+ expr | - expr | E)
--      term   ::= factor (* term | / expr | E)
--      factor ::= (expr) | nat
--      nat    ::= 0 | 1 | 2 | ...
--  NOTE: This grammar does not work when expr is negative,
--        so it's not a "complete calculator parser".
--        Nonetheless it works right for Natural numbers
--        (that was the objective, in the first place).

expr :: Parser Int
expr = (term >>= plusExpr) +++ (term >>= lessExpr) +++ (term >>= epsilon)
       where
           plusExpr = \t -> symbol "+" >> expr >>= \e -> return (t+e)
           lessExpr = \t -> symbol "-" >> expr >>= \e -> return(t-e)
           epsilon  = \t -> return t

term :: Parser Int
term = (factor >>= multiTerm) +++ (factor >>= diviTerm) +++ (factor >>= epsilon)
       where
           multiTerm = \f -> symbol "*" >> term >>= \t -> return (f*t)
           diviTerm  = \f -> symbol "/" >> term >>= \t -> return (f `div` t)
           epsilon   = \f -> return f

factor :: Parser Int
factor = exprWithParens +++ natural
         where
             exprWithParens = symbol "(" >> expr >>= \e -> symbol ")" >> return e
