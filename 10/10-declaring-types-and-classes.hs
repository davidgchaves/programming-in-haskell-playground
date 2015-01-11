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


-- HELPERS for bools function

-- A Type for Bits
type Bit = Int

-- bin2bool: converts a binary number into boolean
bin2bool   :: Bit -> Bool
bin2bool 0 = False
bin2bool 1 = True
-- bin2bool 0 --> False
-- bin2bool 1 --> True

-- int2bin: converts an integer into a "written in reverse" binary number
int2bin   :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)
-- int2bin 13 --> [1,0,1,1]
-- REMEMBER: Binary numbers are written in reverse (from left to right)
--           13 is 1101 in binary, but we write it in reverse 1011

-- make: takes a number of bits filled with 0s if we try to take too many
make :: Int -> [Bit] -> [Bit]
make n bs = take n (bs ++ repeat 0)
-- make 2  [1,0,1] --> [1,0]
-- make 10 [1,0,1] --> [1,0,1,0,0,0,0,0,0,0]

--  The key to generating Substitutions is
--  generating lists of logical values of a given length.
--
--  We are going to interpret Bools as Binary Digits:
--      - False corresponds to 0
--      - True  corresponds to 1
--  Example:
--      - [True, False, True]  corresponds to 101
--      - [False, False, True] corresponds to 001
--
-- bools: produces all possible bool combinations in a list of n-lists,
--        given the n in the n-list
bools   :: Int -> [[Bool]]
bools n = map (map bin2bool . make n . int2bin) [0..limit]
          where
            limit = (2 ^ n) - 1
-- bools 2 --> [[False,False],
--              [True, False],
--              [False,True],
--              [True, True]]

-- bools': the recursive way (much more revealing IMHO)
bools'   :: Int -> [[Bool]]
bools' 0 = [[]]
bools' n = map (False:) bss ++ map (True:) bss
           where bss = bools (n-1)
-- bools' 2 --> [[False,False],
--               [False,True],
--               [True, False],
--               [True, True]]


-- HELPERS for substs function

-- rmdups: removes duplicates from a list
rmdups        :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)
-- rmdups ['A', 'A', 'B', 'A', 'B'] --> "AB" --> ['A', 'B']

-- uniqVars: produces the variables in a Proposition removing duplicates
uniqVars :: Prop -> [Char]
uniqVars = rmdups . vars
-- uniqVars p1 --> "A"  --> ['A']
-- uniqVars p2 --> "AB" --> ['A', 'B']
-- uniqVars p3 --> "AB" --> ['A', 'B']
-- uniqVars p4 --> "AB" --> ['A', 'B']

-- substs:
--  Produces all substitutions for a proposition by
--      1 - extracting its variables
--      2 - removing duplicates from this list
--      3 - generating all possible lists of logical values for this many variables
--      4 - zipping the list of variables with each of the resulting lists
substs   :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = uniqVars p
-- substs p2 --> [[('A',False),('B',False)],
--                [('A',True) ,('B',False)],
--                [('A',False),('B',True)],
--                [('A',True) ,('B',True)]]


-- isTaut: decides if a proposition is a tautology,
--         by evaluating all possible substitutions
isTaut   :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]
-- isTaut p1 --> False
-- isTaut p2 --> True
-- isTaut p3 --> False
-- isTaut p4 --> True


-- 10.5 Abstract Machine for Expressions
--  Specifies the step-by-step process of the expression's evaluation
--
--  Consider:
--  - Expr: simple arithmetic expressions built up from integers using an add operator
--  - value: a function that evaluates such an expression to an integer value
--
--  EXAMPLE:
--      (2 + 3) + 4 ---> value (Add (Add (Val 2) (Val 3)) (Val 4))

-- The Expression Type
data Expr = Val Int
          | Add Expr Expr

-- Example expression
e1 :: Expr
e1 = (Add (Add (Val 2) (Val 3)) (Val 4))

-- value: evaluates expressions to an integer value
value           :: Expr -> Int
value (Val n)   = n
value (Add x y) = value x + value y
-- value e1 --> 9

