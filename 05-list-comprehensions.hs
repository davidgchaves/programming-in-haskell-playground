--
-- Exercise 1: Expression that calculates the sum 1^2 + 2^2 + ... 100^2
--
sumSquares :: Int -> Int
sumSquares n = sum [ x^2 | x <- [1..n] ]
-- sumSquares 100 --> 338350

sumSquares' :: Int -> Int
sumSquares' n = foldl (+) (0) [ x^2 | x <- [1..n] ]
-- sumSquares' 100 --> 338350


--
-- Exercise 2: Reimplement 'replicate' using a list comprehension
--             replicate :: Int -> a -> [a] produces a list of identical elements
--             replicate 3 True --> [True, True, True]
--
replicate' :: Int -> a -> [a]
replicate' n a = [ a | _ <- [1..n] ]
-- replicate' 8 "NaN" --> ["NaN","NaN","NaN","NaN","NaN","NaN","NaN","NaN"]

