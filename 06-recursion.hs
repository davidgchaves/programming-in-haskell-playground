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

