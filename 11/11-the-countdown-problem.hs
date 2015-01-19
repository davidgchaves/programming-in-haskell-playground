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

-- solution: decides if an Expression is a solution to the game
solution              :: Expression -> [Int] -> Int -> Bool
solution exp inps sol = validInputs exp inps && matchTarget exp sol
-- solution e1 [1,3,7,10,25,50] 765 --> True
-- solution e1 [1,3,7,10,25,50] 831 --> False

-- validInputs: checks if a list of values from an Expression is chosen from a given list of numbers
validInputs          :: Expression -> [Int] -> Bool
validInputs exp inps = elem (values exp) (choices inps)
-- validInputs e1 [1,3,7,10,25,50] --> True
-- validInputs e1 [1,3,7,25,50]    --> False

-- matchTarget: checks if an Expression evaluates to the given target/solution
matchTarget         :: Expression -> Int -> Bool
matchTarget exp sol = eval exp == [sol]
-- matchTarget e1 765 --> True
-- matchTarget e1 831 --> False


-- 11.3 Brute force solution
--
--  Solving the countdown problem using the idea of
--  generating all possible Expressions over the given list of numbers

-- split: produces a list of all possible ways of splitting a list
--        into two non-empty lists
split        :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]
-- split [1,2,3,4] --> [([1],[2,3,4]), ([1,2],[3,4]), ([1,2,3],[4])]

-- exprs: produces a list of all possible Expressions
--        whose values are precisely a given list of numbers
--  - For the empty list                ---> there are no possible Expressions
--  - For a single number               ---> there is a single Expression comprising that number
--  - For a list of two or more numbers --->
--      1st: produce all splittings of the list
--      2nd: recursively calculate all possible Expressions for each of these lists
--      3rd: combine each pair of Expressions using each of the four numeric Operators
exprs     :: [Int] -> [Expression]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls,rs) <- split ns
               , l       <- exprs ls
               , r       <- exprs rs
               , e       <- combine l r]

-- combine: produces a list combining a pair of Expressions with the four numeric Operators
combine     :: Expression -> Expression -> [Expression]
combine l r = [App o l r | o <- operators]

-- operators: produces a list with the four numeric Operators
operators :: [Operator]
operators = [Add, Sub, Mul, Div]

-- solutions: produces a list with all possible Expressions that solve
--            an instance of the countdown problem
--      1st: generate all Expressions that are choices from the given list of numbers
--      2nd: brute force all Expressions over each choice
--      3rd: select those Expressions that successfully evaluate to give the target
solutions          :: [Int] -> Int -> [Expression]
solutions inps sol = [exp | inps' <- choices inps
                          , exp   <- exprs inps'
                          , matchTarget exp sol]
-- solutions [1,3,7,10,25,50] 765 -->
--  [ App Mul (Val 3) (App Sub (App Mul (Val 7) (App Sub (Val 50) (Val 10))) (Val 25)),
--    App Mul (App Sub (App Mul (Val 7) (App Sub (Val 50) (Val 10))) (Val 25)) (Val 3),
--    App Mul (Val 3) (App Sub (App Mul (App Sub (Val 50) (Val 10)) (Val 7)) (Val 25)),
--    App Mul (App Sub (App Mul (App Sub (Val 50) (Val 10)) (Val 7)) (Val 25)) (Val 3),
--    App Mul (App Sub (Val 25) (Val 10)) (App Add (Val 1) (Val 50)),
--    ... ]

-- PROBLEMS OF THE BRUTE-FORCE SOLUTION
--
--  1: Many of the Expressions found by solution will fail to evaluate
--     due to the fact that '-' and '/' are not always valid for positive naturals
--  2: Really slow


-- 11.4 Combining generation and evaluation
--
--  Improve our brute force program by combining
--      - the generation of Expressions
--      - with their evaluation
--  such that both tasks are performed simultaneously.
--
--  In this way, Expressions that fail to evaluate are
--      - rejected at an earlier stage
--      - not used to generate further such Expressions

