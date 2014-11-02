import Data.Char
-- We are going to use
--      ord     :: Char -> Int    Ex: ord 'a'     --> 97
--      chr     :: Int  -> Char   Ex: chr 97      --> 'a'
--      isLower :: Char -> Bool   Ex: isLower 'c' --> True
-- from Data.Char

--
-- Encoding and Decoding
--

-- converts a lower-case letter between 'a' and 'z'
-- into the corresponding integer between 0 and 25
lowerChar2Int :: Char -> Int
lowerChar2Int c = ord c - ord 'a'
-- lowerChar2Int 'a' --> 0
-- lowerChar2Int 'z' --> 25

-- converts an integer between 0 and 25
-- into the corresponding lower-case letter between 'a' and 'z'
int2LowerChar :: Int -> Char
int2LowerChar n = chr (n + ord 'a')
-- int2LowerChar 0  --> 'a'
-- int2LowerChar 25 --> 'z'

-- applies a shift factor n to a lower-case letter c
shiftLowerChar :: Int -> Char -> Char
shiftLowerChar n c
    | isLower c = int2LowerChar ((lowerChar2Int c + n) `mod` 26)
    | otherwise = c
-- shiftLowerChar 2 'a' --> 'c'
-- shiftLowerChar 1 'z' --> 'a'

-- encodes a string using a given shift factor
encode :: Int -> [Char] -> [Char]
encode n xs = [ shiftLowerChar n x | x <- xs ]
-- encode 13 "roting thirteen" --> "ebgvat guvegrra"

