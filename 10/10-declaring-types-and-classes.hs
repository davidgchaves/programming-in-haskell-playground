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

