--
-- Exercise 1: Split an even-lengthed list into two halves
--
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

halve' :: [a] -> ([a], [a])
halve' xs = splitAt (div (length xs) 2) xs

halve'' :: [a] -> ([a], [a])
halve'' xs = (take n xs, drop n xs)
    where n = length xs `div` 2

halve''' :: [a] -> ([a], [a])
halve''' xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
    where n = length xs


--
-- Exercise 2: Safetail, like tail but mapping empty list to itself
--

-- Using a conditional expression
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

-- Using guarded equations
safetail' xs
    | null xs   = []
    | otherwise = tail xs

-- Using pattern matching
safetail'' :: [a] -> [a]
safetail'' []     = []
safetail'' (_:xs) = xs

safetail''' :: [a] -> [a]
safetail''' [] = []
safetail''' xs = tail xs

-- Using a lambda
safetail'''' :: [a] -> [a]
safetail''''
    = \ xs ->
        case xs of
            []     -> []
            (_:xs) -> xs


--
-- Exercise 3: Logical disjunction operator (|| i.e. OR) implementations
--
(|||) :: Bool -> Bool -> Bool
False ||| False = False
_     ||| _     = True

(||||) :: Bool -> Bool -> Bool
False |||| b = b
True  |||| _ = True

(|||||) :: Bool -> Bool -> Bool
b ||||| c
    | b == c    = b
    | otherwise = True

(||||||) :: Bool -> Bool -> Bool
b |||||| False = b
_ |||||| True  = True

(|||||||) :: Bool -> Bool -> Bool
b ||||||| c
    | b == c    = c
    | otherwise = True

(||||||||) :: Bool -> Bool -> Bool
False |||||||| False = False
False |||||||| True  = True
True  |||||||| False = True
True  |||||||| True  = True


--
-- Exercise 4: Logical conjunction operator (&& i.e. AND) implementations
--
(&&&) :: Bool -> Bool -> Bool
True &&& True = True
_    &&& _    = False

(&&&&) :: Bool -> Bool -> Bool
a &&&& b = if a then if b then True else False else False

(&&&&&) :: Bool -> Bool -> Bool
a &&&&& b = if a then b else False

(&&&&&&) :: Bool -> Bool -> Bool
a &&&&&& b = if b then a else False


--
-- Exercise 6: Curry 'mult x y z' in terms of lambda expressions
--
mult :: Num a => a -> a -> a -> a
mult x y z = x*y*z

curryMult :: Num a => a -> a -> a -> a
curryMult = \ x -> (\ y -> (\ z -> x*y*z))


--
-- Extra exercise 1: Implement 'remove n xs'
--                   removes the element at position n from the list xs
--
remove :: Int -> [a] -> [a]
remove n xs = take n xs ++ drop (n+1) xs
-- remove 0 [1,2,3,4] --> [2,3,4]
-- remove 2 [1,2,3,4] --> [1,2,4]


--
-- Extra exercise 2: Guess the type
--
e0 = [False, True, False, True]
e0 :: [Bool]

e1 = [[1,2], [3,4]]
e1 :: Num t => [[t]] -- also [[Integer]] or [[Int]]

e2 = [[[1,2,3]], [[4,5,6]]]
e2 :: Num t => [[[t]]] -- also [[[Integer]]] or [[[Int]]]

e3 x = x*2
e3 :: Num a => a -> a

e4 (x,y) = x
e4 :: (a,b) -> a

e5 (x,y,z) = z
e5 :: (a,b,c) -> c

e6 x y = x*y
e6 :: Num a => a -> a -> a

e7 (x,y) = (y,x)
e7 :: (a,b) -> (b,a)

e8 x y = (y,x)
e8 :: a -> b -> (b,a)

e9 [x,y] = (x, True)
e9 :: [t] -> (t, Bool)

e10 (x,y) = [x,x]
e10 :: (t,t) -> [t]


--
-- Extra exercise 3: A suitable definition for the following types
--
e11 :: (Char, Bool)
e11 = ('\a', True)

e12 :: [(Char, Int)]
e12 = [('a', 1)]

e13 :: Int -> Int -> Int
e13 x y = x + y * y

e14 :: ([Char], [Float])
e14 = ("Haskell", [3.1, 3.14, 3.141, 3.1415])

e15 :: [a] -> [b] -> (a,b)
e15 xs ys = (head xs, head ys)

