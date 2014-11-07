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


--
-- Exercise 6: Reimplement elem
--
elem' :: Eq a => a -> [a] -> Bool
elem' _ []                 = False
elem' x (y:ys) | x == y    = True
               | otherwise = elem' x ys
-- elem' 'v' "recursive" --> True
-- elem' 'a' "recursive" --> False


--
-- Exercise 7: Implement merge (given 2 sorted lists)
--
merge :: Ord a => [a] -> [a] -> [a]
merge xs []                     = xs
merge [] ys                     = ys
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y : ys)
                    | otherwise = y : merge (x : xs) ys
-- merge [2, 5, 6] [1, 3, 4] --> [1,2,3,4,5,6]


--
-- Exercise 8: Implement msort (merge sort) using merge and halve
--
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort ys) (msort zs)
    where (ys, zs) = halve xs
-- msort [4,3,6,4,1,7,3,4] --> [1,3,3,4,4,4,6,7]

