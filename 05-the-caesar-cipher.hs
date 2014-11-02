import Data.Char
-- We are going to use
--      ord :: Char -> Int    Ex: ord 'a' --> 97
--      chr :: Int  -> Char   Ex: chr 97  --> 'a'
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

