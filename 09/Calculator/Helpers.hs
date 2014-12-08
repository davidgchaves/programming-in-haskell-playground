module Calculator.Helpers
(
  beep
, cls
, getCh
) where

import System.IO

-- beep: sounds a beep
beep :: IO ()
beep = putStr "\BEL"

-- cls: clears the screen
cls :: IO ()
cls = putStr "\ESC[2J"

-- getCh: reads a character without echoing
getCh :: IO Char
getCh = hSetEcho stdin False >>
        getChar >>= \c ->
        hSetEcho stdin True >>
        return c

