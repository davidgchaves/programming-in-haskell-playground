--
-- Exercise 1: Split an even-lengthed list into two halves
--
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

halve' :: [a] -> ([a], [a])
halve' xs = splitAt (div (length xs) 2) xs

halve'' :: [a] -> ([a], [a])
halve'' xs = (take n xs, drop n xs)
    where n = length xs `div` 2

halve''' :: [a] -> ([a], [a])
halve''' xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
    where n = length xs

