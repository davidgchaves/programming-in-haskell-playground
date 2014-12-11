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

