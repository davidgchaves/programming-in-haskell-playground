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


-- Extra Exercise 09-3:
--  Define a function getLine :: IO String that
--      - reads a line, up to the first \n character, from the standard input
--  You can use getChar :: IO Char that
--      - reads a single character from the standard input
getLine' :: IO String
getLine' = get []

get    :: String -> IO String
get xs = do x <- getChar
            case x of
                '\n' -> return xs
                _    -> get (xs ++ [x])

-- 1: invoke checkGetLine
-- 2: in the new line type the line you want getLine' to get
-- 3: check the result (should be the same)
checkGetLine :: IO ()
checkGetLine = do xs <- getLine'
                  putStrLn' xs


-- Extra Exercise 09-4:
--  Define a function interact :: (String -> String) -> IO () that
--      - takes a function of type String -> String, and
--      - reads a line from the standard input and passes it to this function
--      - and then prints the resulting output followed by a newline on the standard output
interact'   :: (String -> String) -> IO ()
interact' f = do input <- getLine'
                 putStrLn' (f input)
-- check it with interact' reverse for example


-- Extra Exercise 09-5:
--  Define a function sequence_ :: Monad m => [m a] -> m () that
--      - takes a list of monadic values, and
--      - evaluates them in sequence (left to right) ignoring all intermediate results
sequence_'        :: Monad m => [m a] -> m ()
sequence_' []     = return ()
sequence_' (m:ms) = m >> sequence_' ms
-- sequence_' []
-- sequence_' $ map print [1,2,3] --> 1 2 3

sequence_''        :: Monad m => [m a] -> m ()
sequence_'' []     = return ()
sequence_'' (m:ms) = m >>= \_ -> sequence_'' ms
-- sequence_'' []
-- sequence_''' $ map print [1,2,3] --> 1 2 3

sequence_'''        :: Monad m => [m a] -> m ()
sequence_''' []     = return ()
sequence_''' (m:ms) = (foldl (>>) m ms) >> return ()
-- sequence_''' []
-- sequence_''' $ map print [1,2,3] --> 1 2 3

sequence_''''    :: Monad m => [m a] -> m ()
sequence_'''' ms = foldr (>>) (return ()) ms
-- sequence_'''' []
-- sequence_'''' $ map print [1,2,3] --> 1 2 3

