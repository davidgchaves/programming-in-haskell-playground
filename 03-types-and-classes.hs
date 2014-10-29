--
-- Exercise 1: What are the types of the following values?
--
v1 = ['a', 'b', 'c']
v1 :: [Char]

v2 = ('a', 'b', 'c')
v2 :: (Char, Char, Char)

v3 = [(False,'0'), (True,'1')]
v3 :: [(Bool, Char)]

v4 = ([False,True], ['0','1'])
v4 :: ([Bool], [Char])

v5 = [tail, init, reverse]
v5 :: [ [a] -> [a] ]


--
-- Exercise 2: What are the types of the following functions?
--
second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)


--
-- Extra exercise 1: What are the types of the following expressions?
--
e1 = ["False", "True"]
e1 :: [[Char]] -- or [String]

e2 = ([False, True], False)
e2 :: ([Bool], Bool)

e3 = ("1,2", "3,4")
e3 :: ([Char], [Char]) -- or (String, String)

e4 = [(1,True), (0,False)]
e4 :: [(Int, Bool)]

