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
squares'     :: Integer -> Integer -> [Integer]
squares' m n = [x^2 | x <- [n+1..n+m]]
-- squares' 0 0 --> []
-- squares' 0 2 --> []
-- squares' 2 0 --> [1,4]
-- squares' 4 2 --> [9,16,25,36]

sumSquares'   :: Integer -> Integer
sumSquares' x = sum . uncurry squares' $ (x, x)
-- sumSquares' 50 --> 295425

-- ===================================
-- Ex. 8
-- ===================================

-- produces a list of all coordinate pairs on an [0..m] x [0..n] rectangular grid
coords     :: Integer -> Integer -> [(Integer,Integer)]
coords m n = [(x,y) | x <- [0..m], y <- [0..n]]
-- coords 1 1 --> [(0,0),(0,1),(1,0),(1,1)]
-- coords 1 2 --> [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]

