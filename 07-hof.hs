-- 7.2 Processing lists

-- map defined using list comprehensions
map'      :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]
-- map' (+1) [1,3,5,7,9] --> [2,4,6,8,10]

-- map defined using recursion
map''          :: (a -> b) -> [a] -> [b]
map'' _ []     = []
map'' f (x:xs) = f x : map'' f xs
-- map'' (+1) [1,3,5,7,9] --> [2,4,6,8,10]

-- filter defined using list comprehensions
filter'      :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]
-- filter' even [1..10] --> [2,4,6,8,10]

-- filter defined using recursion
filter''        :: (a -> Bool) -> [a] -> [a]
filter'' _ []   = []
filter'' p (x:xs)
    | p x       = x : filter'' p xs
    | otherwise = filter'' p xs
-- filter'' even [1..10] --> [2,4,6,8,10]

-- The functions map and filter are often used together in programs:
--  * with filter being used to select certain elements from a list,
--  * each of which is then transformed using map
-- map and filter example
sumSqrEven    :: [Int] -> Int
sumSqrEven ns = sum (map (^2) (filter even ns))
-- sumSqrEven [1..10] --> 220

-- all defined using recursion
all'            :: (a -> Bool) -> [a] -> Bool
all' _ []       = True
all' p (x:xs)
    | p x       = all' p xs
    | otherwise = False
-- all' (\x -> False) [] --> True
-- all' even [2,4,6,8]   --> True
-- all' even [2,4,5,6,8] --> False

-- any defined using recursion
any'            :: (a -> Bool) -> [a] -> Bool
any' _ []       = False
any' p (x:xs)
    | p x       = True
    | otherwise = any' p xs
-- any' (\x -> True) [] --> False
-- any' odd [2,4,6,8]   --> False
-- any' odd [2,4,5,6,8] --> True

-- takeWhile defined using recursion
takeWhile'      :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x       = x : takeWhile' p xs
    | otherwise = []
-- takeWhile' even []          --> []
-- takeWhile' even [2,4,6,8]   --> [2,4,6,8]
-- takeWhile' even [2,4,5,6,8] --> [2,4]

-- dropWhile defined using recursion
dropWhile'      :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
    | p x       = dropWhile' p xs
    | otherwise = x : xs
-- dropWhile' even []          --> []
-- dropWhile' even [2,4,6,8]   --> []
-- dropWhile' even [2,4,5,6,8] --> [5,6,8]


-- 7.3 The foldr function

-- foldr defined using recursion
foldr'            :: (a -> b -> b) -> b -> [a] -> b
foldr' f v []     = v
foldr' f v (x:xs) = f x (foldr' f v xs)
-- foldr' (+) 0 [1,2,3,4] --> 10

-- It is best to think of the behaviour of 'foldr f v' in a non-recursive manner.
-- As simply replacing:
--  * each cons operator (:) in a list by the function f
--  * the empty list [] by the value v

-- sum defined using foldr
--  * (:) -> (+)
--  * []  -> 0
sum' :: Num a => [a] -> a
sum' = foldr' (+) 0
-- sum' [1,2,3,4] --> 10

-- product defined using foldr
--  * (:) -> (*)
--  * []  -> 1
product' :: Num a => [a] -> a
product' = foldr' (*) 1
-- product' [1,2,3,4] --> 24

-- or defined using foldr
--  * (:) -> (||)
--  * []  -> False
or' :: [Bool] -> Bool
or' = foldr (||) False
-- or' [False, True, False]  --> True
-- or' [False, False, False] --> False

-- and defined using foldr
--  * (:) -> (&&)
--  * []  -> True
and' :: [Bool] -> Bool
and' = foldr (&&) True
-- and' [True, True, True]  --> True
-- and' [True, False, True] --> False

-- length defined using recursion
length'        :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length xs
-- length' [1..10] --> 10

-- naming a lambda
add1ToThe2ndArg     :: Num a => t -> a -> a
add1ToThe2ndArg _ n = 1 + n
-- length defined using foldr
--  * (:) -> add1ToThe2ndArg
--  * []  -> 0
length'' :: [a] -> Int
length'' = foldr add1ToThe2ndArg 0
-- length'' [1..10] --> 10

-- reverse defined using recursion
reverse'        :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]
-- reverse' [1,2,3,4] --> [4,3,2,1]

-- snoc  - NOTE: Read it backwards ;-)
snoc      :: a -> [a] -> [a]
snoc x xs = xs ++ [x]
-- reverse defined using foldr
--  * (:) -> snoc
--  * []  -> []
reverse'' :: [a] -> [a]
reverse'' = foldr snoc []
-- reverse'' [1,2,3,4] --> [4,3,2,1]


-- 7.5 The composition operator

-- composition
composedWith       :: (b -> c) -> (a -> b) -> (a -> c)
f `composedWith` g = \x -> f (g x)

-- twice defined using our custom function composition
twice   :: (a -> a) -> a -> a
twice f = f `composedWith` f
-- twice (\x -> x*3) 2 --> 18

-- twice defined using the library composition operator (.)
twice'   :: (a -> a) -> a -> a
twice' f = f . f
-- twice' (\x -> x*3) 2 --> 18

-- odd defined using the composition operator (.)
odd' :: Integral a => a -> Bool
odd' = not . even
-- odd' 6 --> False
-- odd' 7 --> True

-- sumSqrEven defined using the composition operator (.)
sumSqrEven' :: [Int] -> Int
sumSqrEven' = sum . map (^2) . filter even
-- sumSqrEven' [1..10] --> 220

