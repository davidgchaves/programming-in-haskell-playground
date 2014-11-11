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

