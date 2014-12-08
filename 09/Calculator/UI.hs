module Calculator.UI
(
  buttons
, showbox
, display
) where

import Calculator.Position (writeAt, seqn)

--  q ---> quit
--  c ---> clear the display
--  d ---> delete a character
--  = ---> evaluate an expression
box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

-- Extra characters: Q, C, D, space, escape, backspace, delete, and newline
buttons :: [Char]
buttons = standard ++ extra
          where
            standard = "qcd=0123456789+-*/()"
            extra    = "QCD \ESC\BS\DEL\n"

-- showbox: displays the calculator box in the top-left corner of the screen
showbox :: IO ()
showbox = seqn [writeAt (1,y) xs | (y,xs) <- zip [1..13] box]

-- display: shows a string in the display of the calculator
--          by first clearing the display
--          and then showing the last thirteen characters of the string
display    :: String -> IO ()
display xs = writeAt (3,2) "             " >>
             writeAt (3,2) (reverse (take 13 (reverse xs)))

