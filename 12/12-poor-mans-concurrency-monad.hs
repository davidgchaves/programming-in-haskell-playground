-- Lab 5: a Monad for handling concurrency
--  We are going to simulate concurrent processes by interleaving them.
--  Interleaving implements concurrency by
--      1 - running the first part of one process,
--      2 - suspending it, and then
--      3 - allowing another process to run
module Lab5 where

import Control.Monad

-- To suspend a process, we need to grab its "future" and store it away for later use.
-- Continuations are an excellent way of implementing this.
-- We can change a function into continuation passing style by
-- adding an extra parameter, the continuation, that represents
-- the "future" work that needs to be done after this function terminates.
-- Instead of producing its result directly,
-- the function will now apply the continuation to the result.

-- Given a computation of type Action, a function that
-- uses a continuation with result type a has the following type:
--  (a -> Action) -> Action

-- This type can be read as a function that:
--  - takes as input a continuation function (a -> Action),
--    that specifies how to continue once the result of type a
--    of the current computation is available.
--  - An application f c of this type will call c with its result
--    when it becomes available.

-- Unfortunately, because we want to make (a -> Action) -> Action into a monad,
-- we first need to wrap it into a trivial ADT,
-- which we have to wrap and unwrap when implementing the monad operators:
data Concurrent a = Concurrent ((a -> Action) -> Action)

-- A process is represented by the recursive ADT Action that encodes primitive actions that:
data Action
    = Atom (IO Action)    -- 1 - perform a side-effect and then return a continuation action
    | Fork Action Action  -- 2 - perform the concurrent execution of 2 actions
    | Stop                -- 3 - represent an action that has terminated

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"

-- NOTE: We find it easiest to derive the implementations
--       by ignoring the wrapper (think like a fundamentalist)
--       since that makes "listening to the types" easier,
--       and then add pattern matching and constructor calls
--       to make GHC happy (code like a hacker).

-- ===============================================================
-- Ex. 0 - Implement the function action :: Concurrent a -> Action
-- ===============================================================

--  To express the connection between:
--      - an expression of type Concurrent a, and
--      - an expression of type Action,
--  we define a function action :: Concurrent a -> Action that
--      transforms a ((a -> Action) -> Action) into an Action that
--      uses Stop :: Action to create the continuation
--      to the Concurrent a passed as the first argument to action.

--  The easiest road to implement this function is to
--      - initially ignore the Concurrent wrapper,
--        (define a function action :: ((a -> Action) -> Action) -> Action)
--      - later add the pattern-matching to remove the wrapper
--        (transform a value of type Concurrent a into
--         a value of type ((a -> Action) -> Action) -> Action)

--  As always, let the types guide you:
--      - there is only one obvious way to create
--        a value of type a -> Action from the value Stop :: Action
--      - when you get a value of type ma :: ((a -> Action) -> Action)
--        there is only one way to combine these two to obtain a value of type Action.

action :: Concurrent a -> Action
action = error "You have to implement action"


-- ===================================
-- Ex. 1
-- ===================================

stop :: Concurrent a
stop = error "You have to implement stop"


-- ===================================
-- Ex. 2
-- ===================================

atom :: IO a -> Concurrent a
atom = error "You have to implement atom"


-- ===================================
-- Ex. 3
-- ===================================

fork :: Concurrent a -> Concurrent ()
fork = error "You have to implement fork"

par :: Concurrent a -> Concurrent a -> Concurrent a
par = error "You have to implement par"


-- ===================================
-- Ex. 4
-- ===================================

instance Monad Concurrent where
    (Concurrent f) >>= g = error "You have to implement >>="
    return x = Concurrent (\c -> c x)


-- ===================================
-- Ex. 5
-- ===================================

roundRobin :: [Action] -> IO ()
roundRobin = error "You have to implement roundRobin"

-- ===================================
-- Tests
-- ===================================

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331)
         loop $ genRandom 42
         atom (putStrLn "")


-- ===================================
-- Helper Functions
-- ===================================

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs

