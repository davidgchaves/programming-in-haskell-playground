-- 6.1 Basic Concepts
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)
-- factorial 5 --> 120

prod :: Int -> Int -> Int
m `prod` 0 = 0
m `prod` n = m + (m `prod` (n-1))
-- 5 `prod` 4 --> 20


-- 6.2 Recursion on Lists
product' :: Num a => [a] -> a
product' []     = 1
product' (n:ns) = n * product' ns
-- product' [2,4,6] --> 48

length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs
-- length' [1,2,3,4,5] --> 5

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]
-- reverse' [1,2,3,4,5] --> [5,4,3,2,1]

-- ++ operator
append :: [a] -> [a] -> [a]
[] `append` ys     = ys
(x:xs) `append` ys = x : (xs `append` ys)
-- [1,2] `append` [3,4,5] --> [1,2,3,4,5]

insert :: Ord a => a -> [a] -> [a]
insert x []                 = [x]
insert x (y:ys) | x <= y    = x : y : ys
                | otherwise = y : insert x ys
-- insert 3 [1,2,4,5] --> [1,2,3,4,5]

isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)
-- isort [2,6,3,2,7,1,4,9] --> [1,2,2,3,4,6,7,9]


-- 6.3 Multiple Arguments
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
-- zip' [1,2,3,4,5] ["one","two","three"] --> [(1,"one"), (2,"two"), (3,"three")]

drop' :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' n []     = []
drop' n (_:xs) = drop' (n-1) xs
-- drop' 3 [1,2,3,4,5] --> [4,5]
-- drop' 9 [1,2,3,4,5] --> []


-- 6.4 Multiple Recursion
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-2) + fibonacci (n-1)
-- fibonacci 7 --> 13

