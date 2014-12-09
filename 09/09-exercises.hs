-- Extra Exercise 09-1:
--  Define a function putStr :: String -> IO () that
--      - takes a String and
--      - writes it to the standard output
--  You can use putChar :: Char -> IO () that
--      - takes a character and
--      - writes it to the standard output
putStr'        :: String -> IO ()
putStr' []     = return ()
putStr' (x:xs) = putChar x >> putStr' xs

