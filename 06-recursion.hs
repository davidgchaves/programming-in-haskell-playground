-- 6.1 Basic Concepts
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)
-- factorial 5 --> 120

