--
-- Exercise 1: Reimplement ^ for non-negative integers (including 0)
--
(^^^) :: Int -> Int -> Int
m ^^^ 0 = 1
m ^^^ n = m * m ^^^ (n-1)
-- 3 ^^^ 4 --> 81

(^^^^) :: Int -> Int -> Int
m ^^^^ 0 = 1
m ^^^^ n = m * (^^^^) m (n-1)
-- 3 ^^^^ 4 --> 81


--
-- Exercise 2: Reimplement and
--
and' :: [Bool] -> Bool
and' []     = True
and' (b:bs) = b && and' bs

and'' :: [Bool] -> Bool
and'' []                 = True
and'' (b:bs) | b         = and'' bs
             | otherwise = False

and''' :: [Bool] -> Bool
and''' []                  = True
and''' (b:bs) | b == False = False
              | otherwise  = and''' bs

and'''' :: [Bool] -> Bool
and'''' []     = True
and'''' (b:bs) = and'''' bs && b

