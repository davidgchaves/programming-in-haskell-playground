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


-- Exercise 10-1:
--  Define a function mult :: Nat -> Nat -> Nat that
--  multiplies two natural numbers m and n
--  (Use recursion and the function add :: Nat -> Nat -> Nat)
mult            :: Nat -> Nat -> Nat
mult m Zero     = Zero
mult m (Succ n) = add m (mult m n)
-- mult Zero               (Succ (Succ Zero)) --> Zero
-- mult (Succ (Succ Zero)) Zero               --> Zero
--
-- mult (Succ (Succ Zero))        (Succ (Succ (Succ Zero))) --> Succ (Succ (Succ (Succ (Succ (Succ Zero)))))
-- mult (Succ (Succ (Succ Zero))) (Succ (Succ Zero))        --> Succ (Succ (Succ (Succ (Succ (Succ Zero)))))


-- Exercise 10-2:
--  Define a function occurs :: Integer -> Tree -> Bool that
--  decides if a given integer occurs in a Binary Search Tree
--  (Use the standard library ADT (algebraic data type) Ordering)
data Tree = Leaf Integer
          | Node Tree Integer Tree

bst = Node (Node (Leaf 1) 3 (Leaf 4))
           5
           (Node (Leaf 6) 7 (Leaf 9))

occurs                :: Integer -> Tree -> Bool
occurs m (Leaf n)     = m == n
occurs m (Node l n r) = case compare m n of
                            LT -> occurs m l
                            EQ -> True
                            GT -> occurs m r
-- occurs 1 bst --> True
-- occurs 5 bst --> True
-- occurs 8 bst --> False


-- Extra Exercise 10-4:
--  Define a function occurs :: Integer -> Tree -> Bool that
--  decides if a given integer occurs in a Binary Search Tree
--  (Don't use the standard library ADT (algebraic data type) Ordering)
occurs'                :: Integer -> Tree -> Bool
occurs' m (Leaf n)     = m == n
occurs' m (Node l n r)
    | m == n    = True
    | m < n     = occurs' m l
    | otherwise = occurs' m r
-- occurs' 1 bst --> True
-- occurs' 5 bst --> True
-- occurs' 8 bst --> False


-- Exercise 10-3:
--  Consider the following type of Binary Trees (with only values at the leafs)
data Tree' = Leaf' Integer
           | Node' Tree' Tree'

--  Define a function balanced :: Tree -> Bool that
--  decides if a Tree is (or not) balanced
--  (We say that a tree is balanced if the number of leaves in the left
--  and right subtree of every node differs by at most one)
balTree = Node' (Node' (Leaf' 1) (Leaf' 4))
                (Node' (Leaf' 6) (Leaf' 9))

unbalTree = Node'
                (Node'
                    (Node' (Leaf' 1) (Leaf' 4))
                    (Node' (Leaf' 6) (Leaf' 8)))
                (Leaf' 9)

leaves            :: Tree' -> Integer
leaves (Leaf' _)   = 1
leaves (Node' l r) = leaves l + leaves r

balanced             :: Tree' -> Bool
balanced (Leaf' _)   = True
balanced (Node' l r) = abs (leaves l - leaves r) <= 1 &&
                       balanced l &&
                       balanced r
-- balanced balTree   --> True
-- balanced unbalTree --> False

