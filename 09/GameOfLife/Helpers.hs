module GameOfLife.Helpers
(
  cls
, wait
) where

import GameOfLife.Position (seqn)

-- cls: clears the screen
cls :: IO ()
cls = putStr "\ESC[2J"

-- wait: used to slow down the game to a reasonable speed
--       implemented by performing a given number of dummy actions
wait   :: Int -> IO ()
wait n = seqn [ return() | _ <- [1..n] ]

