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

