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

-- e1: One expression that wins the game from the example at 11.1
--     (25 - 10) * (50 + 1)
e1 :: Expression
e1 = App Mul (App Sub (Val 25) (Val 10)) (App Add (Val 50) (Val 1))

-- eval: produces the overall value of an Expression
--          - the singleton list (that contains the value) means succcess
--          - the empty list means failure
eval             :: Expression -> [Int]
eval (Val n)     = [n           | n > 0]
eval (App o l r) = [apply o x y | x <- eval l
                                , y <- eval r
                                , valid o x y]
-- eval e1 --> [765]

-- values: returns the list of values in an Expression
values             :: Expression -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r
-- values e1 --> [25, 10, 50, 1]

-- subs: produces all subsequences of a list,
--       which are given by all possible combinations of excluding or including each element
subs        :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs
-- subs [1,2,3] --> [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]

-- interleave: produces all possible ways of inserting a new element into a list
interleave          :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)
-- interleave 0 [1,2,3] --> [[0,1,2,3],[1,0,2,3],[1,2,0,3],[1,2,3,0]]

-- perms: produces all permutations of a list,
--        which are given by all possible reorderings of the elements
perms        :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))
-- perms [1,2,3] --> [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

-- choices: produces a list of all possible ways of
--          choosing zero or more elements from an input list
choices    :: [a] -> [[a]]
choices xs = concat (map perms (subs xs))
-- choices [1,2,3] --> [ [],
--                       [3],[2],[1],
--                       [2,3],[3,2],[1,3],[3,1],[1,2],[2,1],
--                       [1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1] ]

