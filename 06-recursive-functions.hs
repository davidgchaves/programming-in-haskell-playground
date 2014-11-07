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


--
-- Exercise 3: Reimplement concat
--
concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss
-- concat' [[], [1,2], [3,4,5]] --> [1,2,3,4,5]


--
-- Exercise 4: Reimplement replicate
--
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x
-- replicate' 3 "ab" --> ["ab","ab","ab"]


--
-- Exercise 5: Reimplement !! (0-based)
--
nth :: [a] -> Int -> a
(x:_) `nth` 0  = x
(_:xs) `nth` n = xs `nth` (n-1)
-- [1,2,3,4,5] `nth` 3 --> 4

