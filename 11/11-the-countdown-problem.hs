-- 11.1 Introduction

-- THE COUNTDOWN PROBLEM
--
--  Given a sequence of numbers
--      like 1 3 7 10 25 50
--  and the arithmetic operators with parentheses
--      + - * / ( )
--  attempt to construct an expression whose value is a given target number
--      like 765
--
--  A possible solution for our example
--      (25 - 10) * (50 + 1)
--
--  NOTES:
--      1: Each number in the sequence can only be used at most once in the expression
--      2: All of the numbers involved must be positive natural numbers
--


-- 11.2 Formalising the problem

-- The Operator Algebraic Data Type
data Operator = Add | Sub | Mul | Div
                deriving Show

