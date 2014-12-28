-- Exercise 13-1:
--  Give some example of functions from the standard library
--  that are defined using overlapping patterns

last'        :: [a] -> a
last' [x]    = x
last' (_:xs) = last' xs

init'        :: [a] -> [a]
init' [_]    = []
init' (x:xs) = x : init' xs

drop'          :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' n []     = []
drop' n (x:xs) = drop' (n-1) xs

