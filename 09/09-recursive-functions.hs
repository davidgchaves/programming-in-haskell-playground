module Lab4 where

------------------------------------------------------------------------------------------------------------------------------
-- RECURSIVE FUNCTIONS
------------------------------------------------------------------------------------------------------------------------------

import Data.Char

-- ===================================
-- Ex. 0
-- Define a recursive function by induction over integers triangle such that
--  triangle n returns the sum of all numbers in the list [0..n], where n >= 0
-- ===================================

triangle   :: Integer -> Integer
triangle 0 = 0
triangle n = n + triangle (n-1)
-- triangle 500 --> 125250

-- ===================================
-- Ex. 1
-- Define a recursive function count that
--  counts how many times a given value occurs in a list
-- ===================================

count           :: Eq a => a -> [a] -> Int
count _ []      = 0
count a (x:xs)
    | a == x    = 1 + count a xs
    | otherwise = count a xs
-- count "Haskell" ["Java", "PHP", "Javascript", "C#"]       --> 0
-- count 'e' "The quick brown fox jumped over the lazy dog." --> 4

xs = [1,2,35,2,3,4,8,2,9,0,5,2,8,4,9,1,9,7,3,9,2,0,5,2,7,6,92,8,3,6,1,9,2,4,8,7,1,2,8,0,4,5,2,3,6,2,3,9,8,4,7,1,4,0,1,8,4,1,2,4,56,7,2,98,3,5,28,4,0,12,4,6,8,1,9,4,8,62,3,71,0,3,8,10,2,4,7,12,9,0,3,47,1,0,23,4,8,1,20,5,7,29,3,5,68,23,5,6,3,4,98,1,0,2,3,8,1]
ys = map (\x -> ((x + 1) * 3) ^ 3 - 7) xs

poem = [ "Three Types for the Lisp-kings under the parentheses,"
       , "Seven for the Web-lords in their halls of XML,"
       , "Nine for C Developers doomed to segfault,"
       , "One for the Dark Lord on his dark throne"
       , "In the Land of Haskell where the Monads lie."
       , "One Type to rule them all, One Type to find them,"
       , "One Type to bring them all and in the Lambda >>= them"
       , "In the Land of Haskell where the Monads lie."
       ]

-- ===================================
-- Ex. 2
-- Define a recursive function that implements euclid
--  - Euclid's Algorithm returns the greatest common factor of two integers n and m
--    where both n and m are greater than 0
--  - The algorithm repeatedly subtracts the smaller number from the larger
--    until the two numbers are equal, and then returns that number
-- ===================================

euclid          :: (Int,  Int) -> Int
euclid (x,y)
    | x > y     = euclid (x-y, y)
    | x < y     = euclid (x, y-x)
    | otherwise = x
-- euclid (5,7) --> 1
-- euclid (4,2) --> 2

-- ===================================
-- Ex. 3
-- Define a recursive function funkyMap that
--  - takes as arguments two functions f and g and a list xs
--  - applies f to all elements at even positions ([0, 2..]) in xs
--  - applies g to all elements at odd positions ([1, 3..]) in xs
-- ===================================

funkyMap              :: (a -> b) -> (a -> b) -> [a] -> [b]
funkyMap _ _ []       = []
funkyMap f g (x:y:xs) = f x : g y : funkyMap f g xs
funkyMap f g (x:xs)   = f x : funkyMap f g xs
-- funkyMap (+10) (+100) [1,2,3,4,5] --> [11,102,13,104,15]

