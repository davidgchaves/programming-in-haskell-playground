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
--          ---> (induction hypothesis) = Succ (Succ (add n m))
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
--          ---> (induction hypothesis) = Succ (add m n)
--          ---> (unapplying (P1))      = add m (Succ n)


-- Exercise 13-4:
--  Show that replicate produces a list with identical elements, by induction on n >= 0
--  (the same as proving that all (== x) (replicate n x) is always true)

replicate'     :: Int -> a -> [a]
replicate' 0 x = []                      -- (1)
replicate' n x = x : replicate' (n-1) x  -- (2)

all'          :: (a -> Bool) -> [a] -> Bool
all' p []     = True                     -- (3)
all' p (x:xs) = p x && all p xs          -- (4)

--  BASE CASE: Show that all (== x) (replicate 0 x) = True
--      all (== x) (replicate 0 x)
--          ---> (applying replicate (1)) = all (== x) []
--          ---> (applying all (3))       = True
--
--  INDUCTIVE CASE: if all (== x) (replicate n x) (INDUCTION HYPOTHESIS) holds,
--                  (which means that all (== x) (replicate n x) = True)
--                  show that all (== x) (replicate (n+1) x) = True
--      all (== x) (replicate (n+1) x)
--          ---> (applying replicate (2)) = all (== x) (x : replicate n x)
--          ---> (applying all (4))       = x == x && all (== x) (replicate n x)
--          ---> (applying ==)            = True && all (== x) (replicate n x)
--          ---> (applying &&)            = all (== x) (replicate n x)
--          ---> (induction hypothesis)   = True


-- Exercise 13-5a:
--  Show that xs ++ [] = xs, by induction on xs

[]     +++ ys = ys               -- (1)
(x:xs) +++ ys = x : (xs +++ ys)  -- (2)

--  BASE CASE: Show that [] ++ [] = []
--      [] ++ []
--          ---> (applying ++ (1)) = []
--
--  INDUCTIVE CASE: if xs ++ [] (INDUCTION HYPOTHESIS) holds,
--                  (which means that xs ++ [] = xs)
--                  show that (x:xs) ++ [] = (x:xs)
--      (x:xs) ++ []
--          ---> (applying ++ (2))      = x : (xs ++ [])
--          ---> (induction hypothesis) = x : xs


-- Exercise 13-5b:
--  Show that xs ++ (ys ++ zs) = (xs ++ ys) ++ zs, by induction on xs

--  BASE CASE: Show that [] ++ (ys ++ zs) = ([] ++ ys) ++ zs
--      [] ++ (ys ++ zs)
--          ---> (applying ++ (1))         = ys ++ zs
--          ---> (unapplying ++ (1) to ys) = ([] ++ ys) ++ zs
--
--  INDUCTIVE CASE: if xs ++ (ys ++ zs) (INDUCTION HYPOTHESIS) holds,
--                  (which means that xs ++ (ys ++ zs) = (xs ++ ys) ++ zs)
--                  show that (x:xs) ++ (ys ++ zs) = ((x:xs) ++ ys) ++ zs
--      (x:xs) ++ (ys ++ zs)
--          ---> (applying ++ (2))      = x : (xs ++ (ys ++ zs))
--          ---> (induction hypothesis) = x : ((xs ++ ys) ++ zs)
--          ---> (unapplying ++ (2))    = (x : (xs ++ ys)) ++ zs
--          ---> (unapplying ++ (2))    = ((x:xs) ++ ys) ++ zs

