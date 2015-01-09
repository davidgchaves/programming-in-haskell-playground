-- 10.1 Type declarations

--  TYPE DECLARATIONS: A new name (SYNONYM) for an existing type
--      - The name of a new type must begin with CAPITAL LETTER
--      - Cannot be recursive
--      - Can be parameterised (with more than one type parameter) by other types

-- Assoc: a type of lookup tables that associate
--        keys (k) of one type to values (v) of another type
--        declared as a list of pairs (k,v)
type Assoc k v = [(k,v)]

lookupTable :: Assoc Integer String
lookupTable = [(1, "watch"), (3, "this"), (1, "space")]
-- lookupTable    --> [(1,"watch"),(3,"this"),(1,"space")]
-- :t lookupTable --> lookupTable :: Assoc Integer String

-- find: returns the first value (v) that is associated
--       with a given key (k) in a lookup table (t)
find     :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']
-- find 3 lookupTable --> "this"
-- find 1 lookupTable --> "watch"


-- 10.2 Data declarations

--  DATA DECLARATIONS: A completely new (opposed to a synonym) type
--      - You need to specify the new values of the type
--
--  CONSTRUCTORS: The new values of the type
--      - The name of a constructor must begin with CAPITAL LETTER
--      - The same constructor name cannot be used in more than one type
--      - Can be used in the same way as built-in types:
--          * Be passed as arguments to functions
--          * Be returned as results form from functions
--          * Be stored in data structures (such as lists)
--          * Be used in patterns (pattern matching)

-- Move: example of data declaration with 4 constructors
data Move = Left' | Right' | Up | Down

type Pos = (Int, Int)
-- move: apply a Move to a Position
move              :: Move -> Pos -> Pos
move Left'  (x,y) = (x-1, y)
move Right' (x,y) = (x+1, y)
move Up     (x,y) = (x,   y+1)
move Down   (x,y) = (x,   y-1)
-- move Left'  (3,3) --> (2,3)
-- move Right' (3,3) --> (4,3)
-- move Up     (3,3) --> (3,4)
-- move Down   (3,3) --> (3,2)

-- moves: apply a list of Moves to a Position
moves          :: [Move] -> Pos -> Pos
moves []     p = p
moves (m:ms) p = moves ms (move m p)
-- moves [Up, Up, Up, Left', Right', Right', Down] (3,3) --> (4,5)

-- flip': flip the direction of a Move
flip'        :: Move -> Move
flip' Left'  = Right'
flip' Right' = Left'
flip' Up     = Down
flip' Down   = Up
-- move (flip' Left') (3,3)           --> (4,3)
-- moves [Up, Up, Up, flip' Up] (3,3) --> (3,5)


--  CONSTRUCTORS:
--      - Can have arguments

-- The type Shape has values of the form:
--  - Circle r, where r is a floating-point number
--  - Rect x y, where x and y are floating-point numbers
data Shape = Circle Float | Rect Float Float

square   :: Float -> Shape
square n = Rect n n

area            :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y
-- area (Circle 5) --> 78.53982
-- area (Rect 4 6) --> 24.0
-- area (square 3) --> 9.0


--  DATA DECLARATIONS:
--      - Can have parameters

-- The Maybe a type is like a list that is:
--  - empty (Nothing) or
--  - has a single value (Just a)
--
-- ALTERNATIVELY
-- We can think of values of type Maybe a as values of type a that may fail:
--  - Nothing represents failure
--  - Just represents success
data Maybe' a = Nothing' | Just' a

-- safediv: safe version of div using Maybe
safediv     :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)
-- safediv 7 0 --> Nothing
-- safediv 7 3 --> Just 2

-- safehead: safe version of head using Maybe
safehead        :: [a] -> Maybe a
safehead []     = Nothing
safehead (x:xs) = Just x
-- safehead []     --> Nothing
-- safehead [1..5] --> Just 1


------------------------------------------
-- ONE WAY TO LOOK AT DATA DECLARATIONS --
------------------------------------------
--
-- data Bool = False | True
--  * data Bool   <--- Abstract base class Bool
--  * False, True <--- Bool subtypes
--  * you cannot create an instance of Bool (it's abstract)
--  * you can only create instances of False and True, but with a twist:
--      - False and True DON'T HAVE their own types
--      - False and True ARE Bool (:t False --> Bool and :t True --> Bool)


-------------------------------------
-- ONE WAY TO LOOK AT CONSTRUCTORS --
-------------------------------------
--
-- data Shape = Circle Float | Rect Float Float
--  * Circle and Rect are 2 Shape Subtypes
--    (REMEMBER: They don't have their own types, they are Shapes)
--  * The Constructor Circle and Rect can be viewed as functions that
--    construct values of type Shape
--      Circle :: Float (r) -> Shape
--      :t Circle --> Circle :: Float -> Shape
--
--      Rect   :: Float (x) -> Float (y) -> Shape
--      :t Rect --> Rect :: Float -> Float -> Shape


-- 10.3 Recursive types

--  DATA DECLARATIONS:
--      - Can be recursive

data Nat = Zero | Succ Nat
           deriving Show

nat2int          :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n
-- nat2int Zero               --> 0
-- nat2int (Succ Zero)        --> 1
-- nat2int (Succ (Succ Zero)) --> 2

int2nat   :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))
-- int2nat 0 --> Zero
-- int2nat 1 --> Succ Zero
-- int2nat 2 --> Succ (Succ Zero)

add            :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)
-- add (int2nat 3) (int2nat 4) --> Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))


data List a = Nil | Cons a (List a)

len             :: List a -> Int
len Nil         = 0
len (Cons _ xs) = 1 + len xs
-- len (Cons 'a' (Cons 'b' (Cons 'c' Nil))) --> 3


-- Binary Trees
data Tree = Leaf Int
          | Node Tree Int Tree

exampleTree :: Tree
exampleTree = Node (Node (Leaf 1) 3 (Leaf 4))
                   5
                   (Node (Leaf 6) 7 (Leaf 9))

-- occurs: decides if a given integer occurs in a tree
occurs                :: Int -> Tree -> Bool
occurs m (Leaf n)     = m == n
occurs m (Node l n r) = m == n || occurs m l || occurs m r
-- occurs 8 exampleTree --> False
-- occurs 4 exampleTree --> True

-- flatten: flattens a tree to a list
flatten              :: Tree -> [Int]
flatten (Leaf n)     = [n]
flatten (Node l n r) = flatten l ++ [n] ++ flatten r
-- flatten exampleTree --> [1,3,4,5,6,7,9]

-- occurs: decides if a given integer occurs in a SEARCH tree
--         NOTE: SEARCH Trees flatten to an ordered list
occurs'            :: Int -> Tree -> Bool
occurs' m (Leaf n) = m == n
occurs' m (Node l n r)
    | m == n = True
    | m < n  = occurs' m l
    | m > n  = occurs' m r
-- occurs' 8 exampleTree --> False
-- occurs' 4 exampleTree --> True

-- A more generic binary tree
data Tree' a = Leaf' a
             | Node' (Tree' a) a (Tree' a)


-- 10.4 Tautology checker

--  Develop a function that decides if simple logical proposition is a tautology
--
--  TAUTOLOGY: Logical proposition that is always true

-- The Proposition Type
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

-- Proposition Examples expressed using the Prop type
-- p1 ---> A AND (NOT A)
p1 :: Prop
p1 = And (Var 'A')
         (Not (Var 'A'))

-- p2 ---> (A AND B) => A
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B'))
           (Var 'A')

-- p3 ---> A => (A AND B)
p3 :: Prop
p3 = Imply (Var 'A')
           (And (Var 'A') (Var 'B'))

-- p4 ---> (A And (A => B)) => B
p4 :: Prop
p4 = Imply (And (Var 'A')
                (Imply (Var 'A') (Var 'B')))
           (Var 'B')

-- The Substitution Type:
--  A lookup table that associates
--      - variable names to
--      - logical values
--  using the Assoc type
type Subst = Assoc Char Bool
-- Example:
--  The substitution [ ('A', False), ('B', True)] assigns
--  the variable A to False, and B to True

-- NOTE: Assoc: a type of lookup tables that associate
--              keys (k) of one type to values (v) of another type
--              declared as a list of pairs (k,v)
--  type Assoc k v = [(k,v)]

-- eval: evaluates a Proposition given a Substitution for its variables
eval               :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

-- vars: produces the variables in a Proposition (does NOT remove duplicates)
vars             :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
-- vars p1 --> "AA"   --> ['A', 'A']
-- vars p2 --> "ABA"  --> ['A', 'B', 'A']
-- vars p3 --> "AAB"  --> ['A', 'A', 'B']
-- vars p4 --> "AABB" --> ['A', 'A', 'B', 'B']


-- HELPERS for bools functions

-- A Type for Bits
type Bit = Int

