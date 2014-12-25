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

