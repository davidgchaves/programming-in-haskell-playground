-- Exercise 13-1:
--  Give some example of functions from the standard library
--  that are defined using overlapping patterns

last'        :: [a] -> a
last' [x]    = x
last' (_:xs) = last' xs

init'        :: [a] -> [a]
init' [_]    = []
init' (x:xs) = x : init' xs

drop'          :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' n []     = []
drop' n (x:xs) = drop' (n-1) xs


-- Exercise 13-2:
--  Show that add n (Succ m) = Succ (add n m), by induction on n
data Nat = Zero
         | Succ Nat

add            :: Nat -> Nat -> Nat
add Zero     m = m               -- (1)
add (Succ n) m = Succ (add n m)  -- (2)

--  BASE CASE: Show that add Zero (Succ m) = Succ (add Zero m)
--      add Zero (Succ m)
--          ---> (applying add (1))        = Succ m
--          ---> (unapplying add (1) to m) = Succ (add Zero m)
--
--  INDUCTIVE CASE: if add n (Succ m) (INDUCTION HYPOTHESIS) holds,
--                  (which means that add n (Succ m) = Succ (add n m))
--                  show that add (Succ n) (Succ m) = Succ (add (Succ n) m)
--      add (Succ n) (Succ m)
--          ---> (applying add (2))     = Succ (add n (Succ m))
--          ---> (induction hypotheses) = Succ (Succ (add n m))
--          ---> (unapplying add (2))   = Succ (add (Succ n) m)


-- Exercise 13-3:
--  Using the properties:
--      add n (Succ m) = Succ (add n m) (P1)
--      add n Zero = n                  (P2)
--  Show that addition is commutative, by induction on n
--  (add n m = add m n)

--  BASE CASE: Show that add Zero m = add m Zero
--      add Zero m
--          ---> (applying add (1)) = m
--          ---> (unapplying (P2))  = add m Zero
--
--  INDUCTIVE CASE: if add n m (INDUCTION HYPOTHESIS) holds,
--                  (which means that add n m = add m n)
--                  show that add (Succ n) m = add m (Succ n)
--      add (Succ n) m
--          ---> (applying add (2))     = Succ (add n m)
--          ---> (induction hypotheses) = Succ (add m n)
--          ---> (unapplying (P1))      = add m (Succ n)

