import Data.Char -- for Exercise 8
--
-- Exercise 1: Expression that calculates the sum 1^2 + 2^2 + ... 100^2
--
sumSquares :: Int -> Int
sumSquares n = sum [ x^2 | x <- [1..n] ]
-- sumSquares 100 --> 338350

sumSquares' :: Int -> Int
sumSquares' n = foldl (+) (0) [ x^2 | x <- [1..n] ]
-- sumSquares' 100 --> 338350


--
-- Exercise 2: Reimplement 'replicate' using a list comprehension
--             replicate :: Int -> a -> [a] produces a list of identical elements
--             replicate 3 True --> [True, True, True]
--
replicate' :: Int -> a -> [a]
replicate' n a = [ a | _ <- [1..n] ]
-- replicate' 8 "NaN" --> ["NaN","NaN","NaN","NaN","NaN","NaN","NaN","NaN"]


--
-- Exercise 3: Implement 'pyths' using a list comprehension
--             'pyths' produces the list of all pythagorean triples (x^2 + y^2 = z^2)
--                     whose components are at most a given limit n
--             pyths 10 --> [(3, 4, 5), (4, 3, 5), (6, 8, 10), (8, 6, 10)]
--
pyths :: Int -> [(Int, Int, Int)]
pyths n = [ (x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2 ]
-- pyths 13 --> [(3,4,5), (4,3,5), (5,12,13), (6,8,10), (8,6,10), (12,5,13)]


--
-- Exercise 4: Implement 'perfects' using a list comprehension
--             'perfects' produces the list of all perfect numbers up to a given limit n
--             A positive integer is perfect if it equals the sum of its factors
--                                           (excluding the number itself)
--             perfects 500 --> [6, 28, 496]
--
factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0 ]
-- factors 6   --> [1,2,3,6]
-- factors 496 --> [1,2,4,8,16,31,62,124,248,496]

perfects :: Int -> [Int]
perfects n = [ x | x <- [1..n], isPerfect x ]
    where isPerfect num = sum (init (factors num)) == num
-- perfects 500 --> [6,28,496]


--
-- Exercise 5: Reimplement e1 using two list comprehensions with single generators
--
e1 = [ (x,y) | x <- [1,2,3], y <- [4,5,6] ]
e2 = concat [ [ (x,y) | y <- [4,5,6] ] | x <- [1,2,3] ]


--
-- Exercise 6: Reimplement 'positions' using 'find'
--             'positions' produces the list of all positions at which a value x occurs
--                         in a list xs
--             positions False [True,False,True,False] --> [1,3]
--
find :: (Eq a) => a -> [(a,b)] -> [b]
find k t = [ v | (k',v) <- t, k == k' ]
-- find 2 [(1,"one"), (2, "two"), (3, "three"), (2, "again")] --> ["two","again"]

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
    where n = length xs - 1
-- positions False [True, True, False, True, False] --> [2,4]


--
-- Exercise 7: Implement 'scalarProduct'
--
scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct xs ys = sum [ x*y | (x,y) <- zip xs ys ]
-- scalarProduct [1,2,3] [4,5,6] --> 32


--
-- Exercise 8: Modify the Caesar cipher program to also handle upper-case letters
--

-- converts a lower-case letter between 'a' and 'z'
-- into the corresponding integer between 0 and 25
lowerChar2Int :: Char -> Int
lowerChar2Int c = ord c - ord 'a'
-- lowerChar2Int 'a' --> 0
-- lowerChar2Int 'z' --> 25

-- converts an upper-case letter between 'A' and 'Z'
-- into the corresponding integer between 0 and 25
upperChar2Int :: Char -> Int
upperChar2Int c = ord c - ord 'A'
-- upperChar2Int 'A' --> 0
-- upperChar2Int 'Z' --> 25

-- converts an integer between 0 and 25
-- into the corresponding lower-case letter between 'a' and 'z'
int2LowerChar :: Int -> Char
int2LowerChar n = chr (n + ord 'a')
-- int2LowerChar 0  --> 'a'
-- int2LowerChar 25 --> 'z'

-- converts an integer between 0 and 25
-- into the corresponding upper-case letter between 'A' and 'Z'
int2UpperChar :: Int -> Char
int2UpperChar n = chr (n + ord 'A')
-- int2UpperChar 0  --> 'A'
-- int2UpperChar 25 --> 'Z'

-- applies a shift factor n to a letter c
shiftChar :: Int -> Char -> Char
shiftChar n c
    | isLower c = int2LowerChar ((lowerChar2Int c + n) `mod` 26)
    | isUpper c = int2UpperChar ((upperChar2Int c + n) `mod` 26)
    | otherwise = c
-- shiftChar 2 'a' --> 'c'
-- shiftChar 1 'z' --> 'a'
-- shiftChar 2 'A' --> 'C'
-- shiftChar 1 'Z' --> 'A'
-- shiftChar 5 '?' --> '?'

-- encodes a string using a given shift factor
encode :: Int -> String -> String
encode n xs = [ shiftChar n x | x <- xs ]
-- encode 13 "Think like a Fundamentalist Code like a Hacker"
--       --> "Guvax yvxr n Shaqnzragnyvfg Pbqr yvxr n Unpxre"

