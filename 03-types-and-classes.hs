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

