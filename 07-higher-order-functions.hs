--
-- Exercise 1: Re-express [f x | x <- xs, p x] using HoF map and filter
--

-- making sense of f and p
f   :: Int -> Int
f x = x^2

p   :: Int -> Bool
p x = even x

-- original list comprehension
e1    :: [Int] -> [Int]
e1 xs = [f x | x <- xs, p x]
-- e1 [1..10] --> [4,16,36,64,100]

-- solution
e2    :: [Int] -> [Int]
e2 xs = map f (filter p xs)
-- e2 [1..10] --> [4,16,36,64,100]

