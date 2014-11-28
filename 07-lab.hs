module Lab3 where

-----------------------------------------------------------------------------------------------------------------------------
-- LIST COMPREHENSIONS
------------------------------------------------------------------------------------------------------------------------------

-- ===================================
-- Ex. 0 - 2
-- ===================================

evens    :: [Integer] -> [Integer]
evens ns = [n | n <- ns, even n]
-- evens [2,5,6,13,32] --> [2,6,32]

-- ===================================
-- Ex. 3 - 4
-- ===================================

-- complete the following line with the correct type signature for this function
squares   :: Integer -> [Integer]
squares n = [x^2 | x <- [1..n]]
-- squares 0 --> []
-- squares 4 --> [1,4,9,16]

sumSquares   :: Integer -> Integer
sumSquares n = sum (squares n)
-- sumSquares 50 --> 42925

-- ===================================
-- Ex. 5 - 7
-- ===================================

-- complete the following line with the correct type signature for this function
-- squares' :: ...
squares' m n = undefined

sumSquares' :: Integer -> Integer
sumSquares' x = sum . uncurry squares' $ (x, x)

-- ===================================
-- Ex. 8
-- ===================================

coords :: Integer -> Integer -> [(Integer,Integer)]
coords = undefined
