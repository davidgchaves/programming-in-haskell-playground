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

