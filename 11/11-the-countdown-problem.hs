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

-- apply: takes two numbers and apply an Operator
apply         :: Operator -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- valid: decides if the application of an Operator conforms the rules of the game
--        (in other words, checks that the result is another positive natural number)
valid         :: Operator -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

-- The Expression Algebraic Data Type
data Expression = Val Int
                | App Operator Expression Expression
                deriving Show

