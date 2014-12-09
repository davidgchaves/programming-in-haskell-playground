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


-- Extra Exercise 09-2:
--  Define a function putStrLn :: String -> IO () that
--      - takes a String and
--      - writes it to the standard output, followed by a newline character
putStrLn'    :: String -> IO ()
putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putStrLn' ""

putStrLn''    :: String -> IO ()
putStrLn'' [] = putChar '\n'
putStrLn'' xs = putStr' xs >> putChar '\n'

putStrLn'''    :: String -> IO ()
putStrLn''' [] = putChar '\n'
putStrLn''' xs = putStr' xs >>= \x -> putChar '\n'

putStrLn''''    :: String -> IO ()
putStrLn'''' [] = putChar '\n'
putStrLn'''' xs = putStr' xs >> putStr' "\n"

