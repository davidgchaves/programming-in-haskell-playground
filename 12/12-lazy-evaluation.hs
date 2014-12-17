-- 12.1 Lazy evaluation intro

-- In Haskell everything uses lazy evaluation unless you make thing strict

-- Expressions are evaluated or reduced by successively applying definitions
-- until no further simplification is possible

-- The basic method of computation in Haskell is
-- the application of functions to arguments
--
-- In Haskell, any two different ways of evaluating the same expression
-- will always produce the same final value, provided that they both terminate


-- 12.2 Evaluation strategies

-- At each stage during evaluation of an expression there may be
-- many possible subexpressions that can be reduced by applying a definition

-- RedEx (Reducible Expression):
--  An expression that has the form of a function applied to one or more arguments
--  (and can be "reduced" by performing the function application)

-- Innermost Evaluation (An Innermost RedEx is always reduced):
--  - Always choose a RedEx that is innermost (it contains no other RedEx),
--    from left to right, in case there's more than one innermost RedEx
--  - Ensures that the argument of a function is always fully evaluated
--    before the function itself is applied => Arguments are passed BY VALUE
--  - Using Innermost Evaluation is normally refered to as CALL-BY-VALUE Evaluation

-- Outermost Evaluation (An Outermost RedEx is always reduced):
--  - Always choose a RedEx that is outermost (it is contained in no other RedEx),
--    from left to right, in case there's more than one outermost RedEx
--  - Allows functions to be applied before their arguments are evaluated
--    => Arguments are passed BY NAME
--  - Using Outermost Evaluation is normally refered to as CALL-BY-NAME Evaluation

-- Reductions and Lambda Expressions:
--  - Functions are viewed as black boxes that we're not permitted to look inside
--    => The selection of RedExes within Lambda Expressions is prohibited
--  - The only operation that can be performed on a function is that
--    of applying it to an argument
--  - Reductions within the body of a function is only permitted
--    once the function has been applied


-- 12.3 Termination

-- - Outermost (call-by-name) Evaluation may produce a result when
--   Innermost (call-by-value) Evaluation FAILS to TERMINATE
--
-- - If there exists any evaluation sequence that terminates for a given expression,
--   then Outermost (call-by-name) Evaluation will also TERMINATE for this expression,
--   and produce the same final result
--
-- - Outermost (call-by-name) Evaluation is preferable to Innermost (call-by-value)
--   for the purpose of ensuring that evaluation TERMINATES as often as possible


-- 12.4 Number of reductions

-- Arguments are evaluated precisely once using Innermost (call-by-value) Evaluation,
--   but may be evaluated many times using Outermost (call-by-name) Evaluation

-- Sharing:
--  - Use pointers to indicate sharing of expressions during evaluation
--  - Solves the Outermost (call-by-name) Evaluation efficiency problem

-- LAZY EVALUATION = Outermost (call-by-name) Evaluation + Sharing
--  - Reduction strategy used by Haskell
--  - Never requires more steps than Innermost (call-by-value) Evaluation


-- 12.5 Infinite structures

-- LAZY EVALUATION:
--  - Only evaluates arguments as and when strictly necessary to produce results
--  - Allows programming with infinite structures (property of Outermost Evaluation)


-- 12.6 Modular Programming
--  Being able to separate control from data
--  is one of the most important benefits of lazy evaluation

-- Infinite Prime Numbers (the sieve of Eratosthenes)
--  1: Write down the infinite sequence (seq) 2,3,4,5...
--  2: Mark the first number (p) in seq as prime
--  3: Delete all multiples of p from seq
--  4: Return to 2
--  NOTE: 1 and 3 require an infinite amount of work
primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
-- take 20 primes         --> [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71]
-- takeWhile (<50) primes --> [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47]

