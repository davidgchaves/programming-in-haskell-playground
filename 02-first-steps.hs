--
-- Exercise 3: Correct the syntatic errors
--
n = a `div` length xs
    where a = 10
          xs = [1,2,3,4,5]

--
-- Exercise 4: Reimplement 'last'
--
ns = [1,2,3,4,5]
cs = "aeiou"

last'   xs = head (drop (length xs - 1) xs)
last''  xs = head (reverse xs)
last''' xs = xs !! (length xs - 1)
-- last ns --> 5
-- last cs --> 'u'

--
-- Exercise 5: Reimplement 'init'
--
init'  xs = take (length xs - 1) xs
init'' xs = reverse (tail (reverse xs))
-- init ns --> [1,2,3,4]
-- init cs --> "aeio"

--
-- Extra exercise 1: Reimplement 'product' for a list of numbers
--
prod :: Num a => [a] -> a
prod []     = 1
prod (x:xs) = x * prod(xs)
-- prod [2,3,4] --> 24

--
-- Extra exercise 2: Implement reverseQuickSort
--
reverseQuickSort :: Ord a => [a] -> [a]
reverseQuickSort []     = []
reverseQuickSort (x:xs) = reverseQuickSort larger ++ [x] ++ reverseQuickSort smaller
    where larger  = [a | a <- xs, a > x]
          smaller = [b | b <- xs, b <= x]
-- reverseQuickSort [3,2,5,6,4,7,11,5,8] --> [11,8,7,6,5,5,4,3,2]

