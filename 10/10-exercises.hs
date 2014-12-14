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


-- Extra Exercise 10-2:
--  Define a function integerToNat :: Integer -> Nat that
--  converts any Integer value >= 0, into the corresponding Nat value
integerToNat   :: Integer -> Nat
integerToNat 0 = Zero
integerToNat n = Succ (integerToNat (n-1))
-- integerToNat 0 --> Zero
-- integerToNat 3 --> Succ (Succ (Succ Zero))

integerToNat'   :: Integer -> Nat
integerToNat' 0 = Zero
integerToNat' n = let m = integerToNat' (n-1) in Succ m
-- integerToNat' 0 --> Zero
-- integerToNat' 3 --> Succ (Succ (Succ Zero))


-- Extra Exercise 10-3:
--  Define a function add :: Nat -> Nat -> Nat that
--  adds two natural numbers m and n
add            :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)
-- add (Succ Zero)        Zero        --> Succ Zero
-- add (Succ (Succ Zero)) (Succ Zero) --> Succ (Succ (Succ Zero))

add'            :: Nat -> Nat -> Nat
add' n Zero     = n
add' n (Succ m) = Succ (add' m n)
-- add' (Succ Zero)        Zero        --> Succ Zero
-- add' (Succ (Succ Zero)) (Succ Zero) --> Succ (Succ (Succ Zero))

