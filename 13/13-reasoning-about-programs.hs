-- 13.2 Reasoning about Haskell

--  Properties of built-in operations in Haskell:
--      The equation x + y = y + x means that
--      for any expressions x and y of the same numeric types,
--      evaluation of x + y and y + x will always produce the same numeric value

--  User-defined functions as equations in Haskell:
double   :: Int -> Int
double x = x + x

--  This equation can also be viewed as a property that
--  can be used when reasoning about this function:
--      For any integer expression x,
--          - the expression double x can freely be replaced by x + x
--          - the expression x + x can freely be replaced by double x .
--  NOTE: When reasoning about programs, function definitions
--      - can be applied from left-to-right
--      - can be unapplied from right-to-left

--  Order in functions defined using multiple equations:
--      1: Equations cannot be viewed as logical properties
--         in isolation from one another
--      2: Equations need to be interpreted in light of the order
--         in which patterns are matched within the equations

--  DISJOINT or NON-OVERLAPPING PATTERNS
--      Patterns that do not rely on the order in which they are matched


-- 13.3 Simple examples

--  REVERSE: Show that reverse has no effect on singleton lists
--           (reverse [x] = [x] for any element x)
reverse'        :: [a] -> [a]
reverse' []     = []                  -- (1)
reverse' (x:xs) = reverse' xs ++ [x]  -- (2)

-- reverse [x]
--  ---> (list notation)        = reverse (x:[])
--  ---> (applying reverse (2)) = reverse [] ++ [x]
--  ---> (applying reverse (1)) = [] ++ [x]
--  ---> (applying ++)          = [x]


--  NEGATE: Show that negate is its own inverse
--          (negate (negate b) = b for all logical values b)
negate'       :: Bool -> Bool
negate' False = True
negate' True  = False

-- Case analysis on the two possible values for b: True and False
--  CASE 1: b = True
--      negate (negate True)
--          ---> (applying the inner negate) = negate False
--          ---> (applying negate)           = True
--  CASE 2: b = False
--      negate (negate False)
--          ---> (applying the inner negate) = negate True
--          ---> (applying negate)           = False


-- 13.4 Induction on numbers

--  Suppose we want to prove that some property, p say, holds for all (finite) natural numbers.
data Nat = Zero
         | Succ Nat

--  Then the PRINCIPLE of INDUCTION states that it is sufficient to show that
--      - BASE CASE: p holds for Zero
--      - INDUCTIVE CASE: if the property p holds for any natural number n (induction hypothesis),
--                        then it also holds for Succ n

-- EXAMPLE 1: Show that add n Zero = n, which we abbreviate by p, holds for all natural numbers n
add            :: Nat -> Nat -> Nat
add Zero     m = m               -- (1)
add (Succ n) m = Succ (add n m)  -- (2)

--  BASE CASE: Show that add Zero Zero = Zero
--      add Zero Zero
--          ---> (applying add (1)) = Zero
--
--  INDUCTIVE CASE: if add n Zero (INDUCTION HYPOTHESIS) holds (which means add n Zero = n)
--                  show that add (Succ n) Zero = Succ n
--      add (Succ n) Zero
--          ---> (applying add (2))     = Succ (add n Zero)
--          ---> (induction hypotheses) = Succ n


-- EXAMPLE 2: Show that addition of natural numbers is associative, which means that
--            add x (add y z) = add (add x y) z for all natural numbers x y z
--      NOTE: The add function is defined by pattern matching on its first argument,
--            so it is natural to try induction on x,
--            which appears twice as the first argument to add in the associativity equation,
--            whereas y only appears once as such and z never

--  BASE CASE: Show that add Zero (add y z) = add (add Zero y) z
--      add Zero (add y z)
--          ---> (applying the outer add (1)) = add y z
--          ---> (unapplying add (1) to y)    = add (add Zero y) z
--
--  INDUCTIVE CASE: if add x (add y z) (INDUCTION HYPOTHESIS) holds
--                  (which means add x (add y z) = add (add x y) z)
--                  show that add (Succ x) (add y z) = add (add (Succ x) y) z
--      add (Succ x) (add y z)
--          ---> (applying outer add (2))   = Succ (add x (add y z))
--          ---> (induction hypothesis)     = Succ (add (add x y) z)
--          ---> (unapplying outer add (2)) = add (Succ (add x y)) z
--          ---> (unapplying inner add (2)) = add (add (Succ x) y) z

