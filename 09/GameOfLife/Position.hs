module GameOfLife.Position
(
  Pos
, goto
, writeAt
, seqn
) where

import System.IO

--  By convention, the position of each character on the screen is given by
--  a pair (x,y) of positive integers, with (1,1) being the top-left corner
type Pos = (Int, Int)

-- goto: moves the cursor to a given position,
--       where the cursor is a marker that indicates
--       where the next character displayed will appear
goto       :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- writeAt: displays a string at a given position
writeAt      :: Pos -> String -> IO ()
writeAt p xs = goto p >> putStr xs

-- seqn: performs a list of actions in sequence,
--       discarding their result values and returning no result
seqn        :: [IO a] -> IO ()
seqn []     = return ()
seqn (a:as) = a >> seqn as

