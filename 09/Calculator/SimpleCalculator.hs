module SimpleCalculator
(
  run
) where

import System.IO

import Calculator.Helpers
import Calculator.Position (goto)
import Calculator.UI
import Calculator.Monad.Parser (parse)
import Calculator.Grammar.ArithmeticExpressions (expr)

-- 9.6 Calculator
--  Simple calculator:
--      - expressions built up from integers using +, -, *, / and ()
--      - allows the user to enter arithmetic expressions interactively using the keyboard
--      - displays the value of such expressions on the screen

-- run: runs the calculator
run :: IO ()
run = cls >> showbox >> clear

-- process: takes a valid character and the current string
--          and performs the appropriate action depending upon the character
process                  :: Char -> String -> IO ()
process c xs
    | elem c "qQ\ESC"    = quit
    | elem c "dD\BS\DEL" = delete xs
    | elem c "=\n"       = eval xs
    | elem c "cC"        = clear
    | otherwise          = press c xs

-- quit: moves the cursor below the calculator box and terminates
quit :: IO ()
quit = goto (1,14)

-- delete: removes the last character from the current string (unless empty string)
--  NOTE: init returns all the elements of a list except the last one
delete    :: String -> IO ()
delete "" = calc ""
delete xs = calc (init xs)

-- eval: displays the result of parsing and evaluating the current string
eval    :: String -> IO ()
eval xs = case parse expr xs of
            [(n,"")] -> calc (show n)
            _        -> beep >> calc xs

-- clear: resets the current string to empty
clear :: IO ()
clear = calc ""

-- press: appends the character to the end of the current string:
press      :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

-- calc: displays the current string
--       and then reads a character from the keyboard without echoing it
calc    :: String -> IO ()
calc xs = display xs >>
          getCh >>= \c ->
          if elem c buttons then process c xs else beep >> calc xs

