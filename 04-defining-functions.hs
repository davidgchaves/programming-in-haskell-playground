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

