-- 12.1 Lazy evaluation intro

-- In Haskell everything uses lazy evaluation unless you make thing strict

-- Expressions are evaluated or reduced by successively applying definitions
-- until no further simplification is possible

-- The basic method of computation in Haskell is
-- the application of functions to arguments
--
-- In Haskell, any two different ways of evaluating the same expression
-- will always produce the same final value, provided that they both terminate

