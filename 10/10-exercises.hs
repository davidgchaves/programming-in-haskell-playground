import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero
         | Succ Nat
         deriving Show

-- Extra Exercise 10-1:
--  Define a function natToInteger :: Nat -> Integer that
--  converts any natural number into the corresponding Integer value
natToInteger          :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = natToInteger n + 1
-- natToInteger Zero                      --> 0
-- natToInteger (Succ (Succ (Succ Zero))) --> 3

natToInteger'          :: Nat -> Integer
natToInteger' Zero     = 0
natToInteger' (Succ n) = 1 + natToInteger' n
-- natToInteger' Zero                      --> 0
-- natToInteger' (Succ (Succ (Succ Zero))) --> 3

natToInteger''       :: Nat -> Integer
natToInteger''       = head . m
    where m Zero     = [0]
          m (Succ n) = [sum [x | x <- (1 : m n)]]
-- natToInteger'' Zero                      --> 0
-- natToInteger'' (Succ (Succ (Succ Zero))) --> 3

natToInteger''' :: Nat -> Integer
natToInteger''' = \n -> genericLength [c | c <- show n, c =='S']
-- natToInteger''' Zero                      --> 0
-- natToInteger''' (Succ (Succ (Succ Zero))) --> 3

