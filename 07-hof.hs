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

