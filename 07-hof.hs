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

