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


--
-- Exercise 2: Reimplement all (for finite, non-partial input lists) using HoFs
--

-- using and and map
all1      :: (a -> Bool) -> [a] -> Bool
all1 p xs = and (map p xs)
-- all1 even [2,4,6,8] --> True
-- all1 even [1..10]   --> False
-- all1 even []        --> True

-- composing and and map
all2   :: (a -> Bool) -> [a] -> Bool
all2 p = and . map p
-- all2 even [2,4,6,8] --> True
-- all2 even [1..10]   --> False
-- all2 even []        --> True

-- composing any and not. A bit convoluted, but it works too.
all3   :: (a -> Bool) -> [a] -> Bool
all3 p = not . any (not . p)
-- all3 even [2,4,6,8] --> True
-- all3 even [1..10]   --> False
-- all3 even []        --> True

-- using foldl
all4      :: (a -> Bool) -> [a] -> Bool
all4 p xs = foldl (&&) True (map p xs)
-- all4 even [2,4,6,8] --> True
-- all4 even [1..10]   --> False
-- all4 even []        --> True

-- composing foldl
all5   :: (a -> Bool) -> [a] -> Bool
all5 p = foldl (&&) True . map p
-- all5 even [2,4,6,8] --> True
-- all5 even [1..10]   --> False
-- all5 even []        --> True

-- using foldr
all6      :: (a -> Bool) -> [a] -> Bool
all6 p xs = foldr (&&) True (map p xs)
-- all6 even [2,4,6,8] --> True
-- all6 even [1..10]   --> False
-- all6 even []        --> True

-- composing foldr
all7   :: (a -> Bool) -> [a] -> Bool
all7 p = foldr (&&) True . map p
-- all7 even [2,4,6,8] --> True
-- all7 even [1..10]   --> False
-- all7 even []        --> True


--
-- Exercise 3: Reimplement any (for finite, non-partial input lists) using HoFs
--

-- using or and map
any1      :: (a -> Bool) -> [a] -> Bool
any1 p xs = or (map p xs)
-- any1 even []        --> False
-- any1 even [1..10]   --> True
-- any1 even [1,3,5,7] --> False

-- composing or and map
any2   :: (a -> Bool) -> [a] -> Bool
any2 p = or . map p
-- any2 even []        --> False
-- any2 even [1..10]   --> True
-- any2 even [1,3,5,7] --> False

-- using filter
any3      :: (a -> Bool) -> [a] -> Bool
any3 p xs = length (filter p xs) > 0
-- any3 even []        --> False
-- any3 even [1..10]   --> True
-- any3 even [1,3,5,7] --> False

-- Absurd
any4   :: (a -> Bool) -> [a] -> Bool
any4 p = not . null . dropWhile (not . p)
-- any4 even []        --> False
-- any4 even [1..10]   --> True
-- any4 even [1,3,5,7] --> False

-- composing not, null and filter
any5   :: (a -> Bool) -> [a] -> Bool
any5 p = not . null . filter p
-- any5 even []        --> False
-- any5 even [1..10]   --> True
-- any5 even [1,3,5,7] --> False

-- convolution FTW!
any6      :: (a -> Bool) -> [a] -> Bool
any6 p xs = not (all (\x -> not (p x)) xs)
-- any6 even []        --> False
-- any6 even [1..10]   --> True
-- any6 even [1,3,5,7] --> False

-- convolution FTW! (v2) (easier to use || instead the lambda)
any7      :: (a -> Bool) -> [a] -> Bool
any7 p xs = foldr (\x acc -> (p x) || acc) False xs
-- any7 even []        --> False
-- any7 even [1..10]   --> True
-- any7 even [1,3,5,7] --> False

-- using foldr (like any7 but revealing intent)
any8      :: (a -> Bool) -> [a] -> Bool
any8 p xs = foldr (||) False (map p xs)
-- any8 even []        --> False
-- any8 even [1..10]   --> True
-- any8 even [1,3,5,7] --> False

-- composing foldr (like any7 but revealing intent)
any9   :: (a -> Bool) -> [a] -> Bool
any9 p = foldr (||) False . map p
-- any9 even []        --> False
-- any9 even [1..10]   --> True
-- any9 even [1,3,5,7] --> False

