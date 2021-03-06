-- 9.3 Basic actions

--  (1) getChar :: IO Char
--      'getChar' reads a character from the keyboard,
--      echoes it to the screen,
--      returns the character as its result value
--
--  (2) putChar :: Char -> IO ()
--      'putChar c' writes the character c to the screen,
--      returns no result value, represented by the empty tuple () (aka Unit)
--
--  (3) return  :: a -> IO ()
--      'return v' returns v without performing any interaction

-- 9.4 Sequencing

--  (1) (>>=)   :: IO a -> (a -> IO b) -> IO b
--      f >>= g = \world -> case f world of
--                              (v,newWorld) -> g v newWorld
--
--      'f >>= g' apply the action f to the current world,
--      then apply the function g to the result value to give a second action,
--      which is then applied to the modified world to give the final result

-- 9.5 Derived primitives
--  Using the three basic 'IO a' actions:
--      getChar, putChar c, return v
--  and sequencing:
--      >>=

-- getLine: reads a string of characters from the keyboard
getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then return []
                           else getLine' >>= \xs -> return (x:xs)

-- putStr: writes a string to the screen
putStr'        :: String -> IO ()
putStr' []     = return ()
putStr' (x:xs) = putChar x >> putStr' xs
-- putStr' "Hello World!" --> Hello World!

-- putStrLn: writes a string to the screen moving on to a new line afterwards
putStrLn'    :: String -> IO ()
putStrLn' xs = putStr' xs >> putChar '\n'
-- putStrLn' "Hello World!" --> Hello World!

-- strlen: action that prompts for a string to be entered from the keyboard,
--         and then displays its length
strlen :: IO ()
strlen = putStr' "Enter a string: " >>
         getLine >>= \xs ->
         putStr' "The string has " >>
         putStr' (show (length xs)) >>
         putStrLn' " characters"
-- strlen
--  --> Enter a string: haskell
--  --> The string has 7 characters

