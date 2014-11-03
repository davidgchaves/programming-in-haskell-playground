--
-- Exercise 1: Expression that calculates the sum 1^2 + 2^2 + ... 100^2
--
sumSquares :: Int -> Int
sumSquares n = sum [ x^2 | x <- [1..n] ]
-- sumSquares 100 --> 338350

sumSquares' :: Int -> Int
sumSquares' n = foldl (+) (0) [ x^2 | x <- [1..n] ]
-- sumSquares' 100 --> 338350

