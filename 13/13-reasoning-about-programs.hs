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


-- EXAMPLE 3: Show that replicate does produce a list with n elements, which means that
--            length (replicate n x) = n for all n >= 0
replicate'     :: Int -> a -> [a]
replicate' 0 _ = []                      -- (1)
replicate' n x = x : replicate' (n-1) x  -- (2)

--  BASE CASE: Show that length (replicate 0 x) = 0
--      length (replicate 0 x)
--          ---> (applying replicate (1)) = length []
--          ---> (applying length)        = 0
--
--  INDUCTIVE CASE: if length (replicate n x) (INDUCTION HYPOTHESIS) holds
--                  (which means length (replicate n x) = n)
--                  show that (replicate (n+1) x) = n+1
--      length (replicate (n+1) x)
--          ---> (applying replicate (2)) = length (x : replicate n x)
--          ---> (applying length)        = 1 + length (replicate n x)
--          ---> (induction hypothesis)   = 1+n
--          ---> (+ commutativity)        = n+1


-- 13.5 Induction on lists

-- Induction is not restricted to natural numbers,
-- but can also be used to reason about other recursive types,
-- such as the type of lists

--  Suppose we want to prove that some property p holds for all lists
--
--  Then the PRINCIPLE of INDUCTION states that it is sufficient to show that
--      - BASE CASE: p holds for the empty list []
--      - INDUCTIVE CASE: if the property p holds for any list xs (induction hypothesis),
--                        then it also holds for x : xs
--  NOTE: Both the element x and the list xs must be of the appropriate types

--  EXAMPLE: Show that reverse is its own inverse, which means that
--             reverse (reverse xs) = xs (by induction on xs)
reverse''        :: [a] -> [a]
reverse'' []     = []                   -- (1)
reverse'' (x:xs) = reverse'' xs ++ [x]  -- (2)

--  BASE CASE: Show that reverse (reverse []) = []
--      reverse (reverse [])
--          ---> (applying inner reverse (1)) = reverse []
--          ---> (applying reverse)           = []
--
--  INDUCTIVE CASE: if reverse (reverse xs) (INDUCTION HYPOTHESIS) holds
--                  (which means reverse (reverse xs) = xs)
--                  show that reverse (reverse (x:xs)) = x:xs
--      reverse (reverse (x:xs))
--          ---> (applying inner reverse (2))      = reverse (reverse xs ++ [x])
--          ---> (++ distributivity (NOTE 1))      = reverse [x] ++ reverse (reverse xs)
--          ---> (induction hypothesis)            = reverse [x] ++ xs
--          ---> (reverse singleton list (NOTE 2)) = [x] ++ xs
--          ---> (applying ++)                     = x:xs
--
--      NOTE 1: ++ distributivity property:
--          reverse (xs ++ ys) = reverse ys ++ reverse xs
--
--      NOTE 2: reverse a singleton list property:
--          reverse [x] = [x]
--


-- 13.6 Making append vanish

-- The efficiency problem of append (++):
--  - The append operator ++ carries a considerable efficiency cost
--    when used recursively
--  - It makes the reverse function to take quadratic time
--    in the length of its argument
--  - Example: reversing a list with 10.000 elements
--             will take about 50.000.000 reduction steps

--  reVerse: our attempt to define a more general function, which
--           combines the behaviours of reverse and ++
--
--  reVerse :: [a] -> [a] -> [a]

--  Show that reVerse produces the reversing of the 1st and 2nd lists appended
--  which means that reVerse xs ys = reverse xs ++ ys (3)

reverse'''        :: [a] -> [a]
reverse''' []     = []                    -- (1)
reverse''' (x:xs) = reverse''' xs ++ [x]  -- (2)

--  BASE CASE: Show that reVerse [] ys = ?
--      reVerse [] ys
--          ---> (applying (3))         = reverse [] ++ ys
--          ---> (applying reverse (1)) = [] ++ ys
--          ---> (applying ++)          = ys
--      So, we have reVerse [] ys = ys (DEF-1)
--
--  INDUCTIVE CASE: if reVerse xs ys (INDUCTION HYPOTHESIS) holds
--                  (which means reVerse xs ys = reverse xs ++ ys)
--                  show that reVerse (x:xs) ys = ?
--      reVerse (x:xs) ys
--          ---> (applying (3))                         = reverse (x:xs) ++ ys
--          ---> (applying reverse (2))                 = (reverse xs ++ [x]) ++ ys
--          ---> (++ associativity)                     = reverse xs ++ ([x] ++ ys)
--          ---> (induction hypothesis (right-to-left)) = reVerse xs ([x] ++ ys)
--          ---> (applying ++)                          = reVerse xs (x:ys)
--      So, we have reVerse (x:xs) ys = reVerse xs (x:ys) (DEF-2)

-- Using DEF-1 and DEF-2 we have our reVerse definition
reVerse           :: [a] -> [a] -> [a]
reVerse []     ys = ys                 -- DEF-1
reVerse (x:xs) ys = reVerse xs (x:ys)  -- DEF-2


--  Suppose we want to prove that some property p holds for all Trees
data Tree = Leaf Int
          | Node Tree Tree

--  Then the PRINCIPLE of INDUCTION states that it is sufficient to show that
--      - BASE CASE: p holds for all trees of the form Leaf n
--      - INDUCTIVE CASE: if the property p holds for any trees l and r (induction hypothesis),
--                        then it also holds for Node l r

-- flatTen: our attempt to produce an efficient flatten on Trees,
--          making ++ vanish

--  Show that flatTen combines flatten and ++, so that
--      flatTen t ns = flatten t ++ ns (3)

flatten            :: Tree -> [Int]
flatten (Leaf n)   = [n]                     -- (1)
flatten (Node l r) = flatten l ++ flatten r  -- (2)

--  BASE CASE: Show that flatTen (Leaf n) ns = ?
--      flatTen (Leaf n) ns
--          ---> (applying (3))         = flatten (Leaf n) ++ ns
--          ---> (applying flatten (1)) = [n] ++ ns
--          ---> (applying ++)          = n:ns
--      So, we have flatTen (Leaf n) ns = n:ns (DEF-3)
--
--  INDUCTIVE CASE: if flatTen t ns (INDUCTION HYPOTHESIS) holds
--                  (which means flatTen l ns = flatten l ++ ns
--                           and flatTen r ns = flatten r ++ ns)
--                  show that flatTen (Node l r) ns = ?
--      flatTen (Node l r) ns
--          ---> (applying (3))                               = flatten (Node l r) ++ ns
--          ---> (applying flatten (2))                       = (flatten l ++ flatten r) ++ ns
--          ---> (++ associativity)                           = flatten l ++ (flatten r ++ ns)
--          ---> (induction hypothesis for l (right-to-left)) = flatTen l (flatten r ++ ns)
--          ---> (induction hypothesis for r (right-to-left)) = flatTen l (flatTen r ns)
--      So, we have flatTen (Node l r) ns = flatTen l (flatTen r ns) (DEF-4)

-- Using DEF-3 and DEF-4 we have our flatTen definition
flatTen               :: Tree -> [Int] -> [Int]
flatTen (Leaf n)   ns = n:ns
flatTen (Node l r) ns = flatTen l (flatTen r ns)


-- 13.7 Compiler correctness

-- Arithmetic Expressions Type: Built up from integers using an addition operator
data Expression = Val Int
                | Add Expression Expression
                deriving Show

-- Such Arithmetic Expressions can be evaluated directly or indirectly
--
-- DIRECTLY
--
-- eval: evaluates an expression directly to an integer value
eval           :: Expression -> Int
eval (Val n)   = n
eval (Add x y) = eval x + eval y


-- INDIRECTLY (by means of Code that executes using a Stack)
--
-- A Stack is just a list of Integers
type Stack = [Int]

-- Code comprises a list of PUSH and ADD Operations on the Stack
type Code  = [Operation]

data Operation = PUSH Int
               | ADD
               deriving Show

-- exec: Executes a piece of Code using an initial Stack to give a final Stack
exec                          :: Code -> Stack -> Stack
exec []           s           = s                   -- (1)
exec (PUSH n : c) s           = exec c (n     : s)  -- (2)
exec (ADD    : c) (m : n : s) = exec c (n + m : s)  -- (3)
--  (1) When there's no more Code, we return s (the Stack)
--  (2) The PUSH Operation places n (a new integer) on the top of s (the Stack)
--  (3) The ADD Operation replaces n and m (the top two integers)
--                        by their sum (n+m) on the top of s (the Stack)

-- comp: Compiles an Expression into Code
comp           :: Expression -> Code
comp (Val n)   = [PUSH n]                   -- (4)
comp (Add x y) = comp x ++ comp y ++ [ADD]  -- (5)
--  (4) n (an integer value) is compiled by PUSHing n into the Stack
--  (5) Add x y is compiled by:
--      1st: compiling x and y (the two argument expressions)
--      2nd: ADDing the resulting two integers on the Stack
--
--  NOTE: when an add operation is performed,
--        the value of expression y will be the top of the stack,
--        and the value of x will be the second top,
--        hence the swapping of these two values in the definition of exec (3)

-- EXAMPLE: Using the expression (2 + 3) + 4
e1 :: Expression
e1 = Add (Add (Val 2) (Val 3)) (Val 4)
-- eval e1           --> 9
-- comp e1           --> [PUSH 2, PUSH 3, ADD, PUSH 4, ADD]
-- exec (comp e1) [] --> [9]


--
-- REASONING ABOUT THE CORRECTNESS OF OUR COMPILER FOR EXPRESSIONS
--

--  RatCooCfE: THE PROBLEM
--
--  Generalising from the example, the correctness of our compiler for expressions
--  can be expressed by the following equation:
--
--      exec (comp e) [] = [eval e]
--
--   Left-hand side equals:
--      compiling an expression e and then executing the resulting code using an empty initial stack
--
--   Right-hand side equals:
--      evaluating the expression e and then converting the resulting integer into a singleton stack
--

--  RatCooCfE: The PRINCIPLE of INDUCTION for Expressions
--
--  data Expression = Val Int
--                  | Add Expression Expression
--
--  Suppose we want to prove that some property p holds for all Expressions
--
--  Then the PRINCIPLE of INDUCTION states that it is sufficient to show that
--      - BASE CASE: p holds for all expressions of the form Val Int
--      - INDUCTIVE CASE: if the property p holds for any expressions x and y (induction hypothesis),
--                        then it also holds for Add x y
--

--  RatCooCfE: The PRINCIPLE of INDUCTION for (Expressions vs Trees)
--
--  The Principle of Induction for Expressions it's the same as
--  the Principle of Induction for Trees but with different constructors:
--      (TREES) Leaf Int       ---> (EXPRESSIONS) Val Int
--      (TREES) Node Tree Tree ---> (EXPRESSIONS) Add Expression Expression
--

--  RatCooCfE: Generalising the Problem to an Arbitrary Initial Stack
--
--  The compiler correctness equation can be verified as follows:
--      exec (comp e) [] = [eval e]
--
--  or generalising from [] (empty initial stack) to s (arbitrary initial stack):
--      exec (comp e) s  = eval e : s
--

--  RatCooCfE: Proving correctness using Induction
--
--  Show that the compiler correctness equation
--      exec (comp e) s  = eval e : s
--  holds.
--
--  BASE CASE: Show that exec (comp (Val n)) s = eval (Val n) : s
--      exec (comp (Val n)) s
--          ---> (applying comp (4))   = exec [PUSH n] s
--          ---> (applying exec (2))   = exec [] (n : s)
--          ---> (applying exec (1))   = n : s
--          ---> (unapplying eval (6)) = eval (Val n) : s
--      So, we have exec (comp (Val n)) s = eval (Val n) : s
--
--  NOTE: Used in steps above
--      exec []           s = s               -- (1)
--      exec (PUSH n : c) s = exec c (n : s)  -- (2)
--      comp (Val n)        = [PUSH n]        -- (4)
--      eval (Val n)        = n               -- (6)
--
--
--  INDUCTIVE CASE: if exec (comp e) s = eval e : s
--                  (which means exec (comp x) s = eval x : s
--                           and exec (comp y) s = eval y : s)
--                  show that exec (comp (Add x y)) = eval (Add x y) : s
--      exec (comp (Add x y)) s
--          ---> (applying comp (5))          = exec (comp x ++ comp y ++ [ADD]) s
--          ---> (++ associativity)           = exec (comp x ++ (comp y ++ [ADD]) s
--          ---> (++ distributivity (8))      = exec (comp y ++ [ADD]) (exec (comp x) s)
--          ---> (induction hypothesis for x) = exec (comp y ++ [ADD]) (eval x : s)
--          ---> (++ distributivity (8))      = exec [ADD] (exec (comp y) (eval x : s))
--          ---> (induction hypothesis for y) = exec [ADD] (eval y : eval x : s)
--          ---> (applying exec (3))          = exec [] ((eval x + eval y) : s)
--          ---> (applying exec (1))          = (eval x + eval y) : s
--          ---> (unapplying eval (7))        = eval (Add x y) : s
--      So, we have exec (comp (Add x y)) x = eval (Add x y) : s
--
--  NOTE: Used in steps above
--      exec []        s           = s                          -- (1)
--      exec (ADD : c) (m : n : s) = exec c (n + m : s)         -- (3)
--      comp (Add x y)             = comp x ++ comp y ++ [ADD]  -- (5)
--      eval (Add x y)             = eval x + eval y            -- (7)
--      exec (c ++ d) s            = exec d (exec c s)          -- (8)
--

--  RatCooCfE: The need for an Arbitrary Initial Stack
--
--  The induction hypothesis for y wouldn't be applicable if the initial stack were empty,
--  (the initial stack = eval x : s  (non-empty))
--          exec [ADD] (exec (comp y) (eval x : s))
--          ---> (induction hypothesis for y) = exec [ADD] (eval y : eval x : s)
--
--  That's why we needed to generalise the equation from an empty stack to an arbitrary stack
--

--  RatCooCfE: The ++ distributivity property
--
--  The distributivity property used in the inductive case states that
--        executing two pieces of code appended together gives the same result as
--        executing the two pieces of code in sequence:
--      exec (c ++ d) s = exec d (exec c s)
--

--  RatCooCfE: Proving the ++ distributivity property by induction
--
--  Show that the ++ distributivity property
--      exec (c ++ d) s = exec d (exec c s)
--  holds, by induction on the code c
--
--  BASE CASE: Show that exec ([] ++ d) s = exec d (exec [] s)
--      exec ([] ++ d) s
--          ---> (applying ++)         = exec d s
--          ---> (unapplying exec (1)) = exec d (exec [] s)
--      So, we have exec ([] ++ d) s = exec d (exec [] s)
--
--  NOTE: Used in steps above
--      exec [] s = s  -- (1)
--
--
--  INDUCTIVE CASE: if exec (c ++ d) s = exec d (exec c s)
--                  show that exec ((PUSH n : c) ++ d) s = exec d (exec (PUSH n : c) s)
--                  and       exec ((ADD    : c) ++ d) s = exec d (exec (ADD    : c) s)
--
--      INDUCTIVE CASE 1: exec ((PUSH n : c) ++ d) s = exec d (exec (PUSH n : c) s)
--      exec ((PUSH n : c) ++ d) s
--          ---> (applying ++)               = exec (PUSH n : (c ++ d) s
--          ---> (applying exec (2))         = exec (c ++ d) (n : s)
--          ---> (induction hypothesis)      = exec d (exec c (n : s))
--          ---> (unapplying inner exec (2)) = exec d (exec (PUSH n : c) s)
--      So, we have exec ((PUSH n : c) ++ d) s = exec d (exec (PUSH n : c) s)
--
--      NOTE: Used in steps above
--      exec (PUSH n : c) s = exec c (n : s)  -- (2)
--
--      INDUCTIVE CASE 2: exec ((ADD : c) ++ d) s = exec d (exec (ADD : c) s)
--      exec ((ADD : c) ++ d) s
--          ---> (applying ++)                 = exec (ADD : (c ++ d) s
--          ---> (assume s of the form m:n:s') = exec (ADD : (c ++ d) (m : n : s')
--          ---> (applying exec (3))           = exec (c ++ d) (n + m : s')
--          ---> (induction hypothesis)        = exec d (exec c (n + m : s'))
--          ---> (unapplying inner exec (3))   = exec d (exec (ADD : c) (m : n : s')
--      So, we have exec ((ADD : c) ++ d) s = exec d (exec (ADD : c) s)
--                  assuming s of the form (m : n : s')
--
--      NOTE: Used in steps above
--      exec (ADD : c) (m : n : s) = exec c (n + m : s)  -- (3)
--

--  RatCooCfE: A Stack Underflow Error
--
--  While proving the Inductive Case 2 we made an assumption
--      s should be of the form (m : n : s')
--
--  This wouldn't hold in case of a stack underflow error
--
--  In practice, this will never arise, because the structure of the compiler
--  ensures that the stack will always contain at least two integers
--  when an add operation is performed
--

--  RatCooCfE: Deriving a '++'-free new Compiler using Induction
--
--  We seek to define a generalised function comp' with the following property:
--      comp' e c = comp e ++ c
--  that way we eleiminate the use of ++ and the consequent Stack Underflow Error

--  Show that comp' combines flatten and ++, so that
--      comp' e c = comp e ++ c  -- (1)
--
--  BASE CASE: Show that comp' (Val n) c = ?
--      comp' (Val n) c
--          ---> (applying (1))      = comp (Val n) ++ c
--          ---> (applying comp (4)) = [PUSH n] ++ c
--          ---> (applying ++)       = PUSH n : c
--      So, we have comp' (Val n) c = PUSH n : c (DEF-1)
--
--  NOTE: Used in steps above
--      comp' e c    = comp e ++ c  -- (1)
--      comp (Val n) = [PUSH n]     -- (4)
--
--  INDUCTIVE CASE: if comp' e c (INDUCTION HYPOTHESIS) holds
--                  (which means comp' x c = comp x ++ c
--                           and comp' y c = comp y ++ c)
--                  show that comp' (Add x y) c = ?
--      comp' (Add x y) c
--          ---> (applying (1))                               = comp (Add x y) ++ c
--          ---> (applying comp (5))                          = (comp x ++ comp y ++ [ADD]) ++ c
--          ---> (++ associativity)                           = comp x ++ comp y ++ ([ADD] ++ c)
--          ---> (applying ++)                                = comp x ++ comp y ++ (ADD : c)
--          ---> (++ associativity)                           = comp x ++ (comp y ++ (ADD : c))
--          ---> (induction hypothesis for y (right-to-left)) = comp x ++ (comp' y (ADD : c))
--          ---> (induction hypothesis for x (right-to-left)) = comp' x (comp' y (ADD : c))
--      So, we have comp' (Add x y) c = comp' x (comp' y (ADD : c) (DEF-2)
--
--  NOTE: Used in steps above
--      comp' e c      = comp e ++ c                -- (1)
--      comp (Add x y) = comp x ++ comp y ++ [ADD]  -- (5)
--

