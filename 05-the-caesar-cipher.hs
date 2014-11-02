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

-- decodes a string using a given shift factor
decode :: Int -> [Char] -> [Char]
decode n xs = [ shiftLowerChar (-n) x | x <- xs ]
-- decode 13 "ebgvat guvegrra" --> "roting thirteen"


--
-- Frequency Tables
--

-- frequency table of the 26 letters of the alphabet (English)
freqTable :: [Float]
freqTable = [ 8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
              6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1 ]

-- calculates the percentage of one integer with respect to another
percent :: Int -> Int -> Float
percent n m = (int2Float n / int2Float m) * 100
              where int2Float n = fromInteger (toInteger n)

-- returns the number of lower-case letters in string xs
lowers :: [Char] -> Int
lowers xs = length [ x | x <- xs, isLower x ]

-- counts the number of ocurrences of char x in string xs
count :: Char -> [Char] -> Int
count x xs = length [ x' | x' <- xs, x == x' ]

-- produces a frequency table like freqTable for string xs
freqs :: String -> [Float]
freqs xs = [ percent (count x xs) n | x <- ['a'..'z'] ]
           where n = lowers xs
-- freqs "abbcccdddeee" --> [8.333334,16.666668,25.0,25.0,25.0,0.0,0.0,...,0.0]

