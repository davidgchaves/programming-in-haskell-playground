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

action'   :: ((a -> Action) -> Action) -> Action
action' f = f (\a -> Stop)

action                :: Concurrent a -> Action
action (Concurrent f) = f (\a -> Stop)
-- action (Concurrent (\a -> Stop))                                   --> stop
-- action (Concurrent (\a -> Fork Stop $ Fork Stop Stop))             --> fork stop fork stop stop
-- action (Concurrent (\a -> Atom $ putStr "Haskell" >> return Stop)) --> atom


--  To make the constructors of the data type Action easily accessible,
--  we can define helper functions that hide the boilerplate required to use them:
--      - (Ex. 1) stop :: Concurrent a
--      - (Ex. 2) atom :: IO a -> Concurrent a
--      - (Ex. 3) fork :: Concurrent a -> Concurrent ()

-- ==========================================================
-- Ex. 1 - Implement the helper function stop :: Concurrent a
-- ==========================================================

--  The function stop :: Concurrent a, discards any continuation (ending a computation):
--      - takes a continuation, which gets discarded
--      - returns a Stop action
--
--  Technically, we need to return a function of type ((a -> Action) -> Action)
--  (wrapped in the Concurrent data type) that ends the computation

stop' :: ((a -> Action) -> Action)
stop' = \c -> Stop

stop :: Concurrent a
stop = Concurrent (\c -> Stop)
-- action stop --> stop


-- ==================================================================
-- Ex. 2 - Implement the helper function atom :: IO a -> Concurrent a
-- ==================================================================

--  The function atom :: IO a -> Concurrent a,
--      - turns an arbitrary computation in the IO Monad
--      - into an atomic action represented using the Atom constructor

--  REMEMBER: The easiest road to implement this function is to
--      - initially ignore the Concurrent wrapper,
--          1: define a function atom :: IO a -> ((a -> Action) -> Action))
--          2: take a value x of type IO a
--          3: return a value of type ((a -> Action) -> Action)
--             which looks like \c -> value-of-type-action
--             where c :: (a -> Action)
--                   value-of-type-action could be Atom
--          4: use (>>=) to combine
--              - a value of type IO a and
--              - a function of type a -> IO b into
--              - a value of type IO b
--                (in this case b is instantiated to Action)
--          5: use return to convert
--              - a value of type Action into
--              - a value of type IO Action
--          6: use the Atom Constructor (check 3) to turn
--              - a value of type IO Action into
--              - an Action
--      - later wrap and unwrap the Concurrent data type

atom'   :: IO a -> ((a -> Action) -> Action)
atom' x = \c -> Atom (x >>= \a -> return (c a))
--atom' x = \c -> Atom (do a <- x
--                         return (c a))

atom   :: IO a -> Concurrent a
atom x = Concurrent (\c -> Atom (x >>= \a -> return (c a)))
--atom x = Concurrent (\c -> Atom (do a <- x
--                                    return (c a)))

-- action . atom . putStrLn $ "Haskell" --> atom
-- action $ atom undefined              --> atom


-- ===================================
-- Ex. 3 - Implement the helper functions:
--          fork :: Concurrent a -> Concurrent ()
--          par  :: Concurrent a -> Concurrent a -> Concurrent a
-- ===================================

--  fork :: Concurrent a -> Concurrent ()
--      - forks its argument by turning it into an action and
--      - continues by passing () as the input to the continuation

fork'   :: ((a -> Action) -> Action) -> ((() -> Action) -> Action)
fork' f = \c -> Fork (action' f) (c ())

fork                :: Concurrent a -> Concurrent ()
fork (Concurrent f) = Concurrent (\c -> Fork (action (Concurrent f)) (c ()))
-- action $ fork stop                     --> fork stop stop
-- action (fork (atom (putStr "Hacker"))) --> fork atom stop


--  par :: Concurrent a -> Concurrent a -> Concurrent a
--  combines two computations into one by
--      - forking them both and
--      - passing the given continuation to both parts

par'     :: ((a -> Action) -> Action) -> ((a -> Action) -> Action) -> ((a -> Action) -> Action)
par' f g = \c -> Fork (f c) (g c)

par                               :: Concurrent a -> Concurrent a -> Concurrent a
par (Concurrent f) (Concurrent g) = Concurrent (\c -> Fork (f c) (g c))
-- action $ par stop stop                                      --> fork stop stop
-- action (par (atom (putStr "think")) (atom (putStr "hack"))) --> fork atom atom
-- action (par stop $ fork stop)                               --> fork stop fork stop stop
-- action $ par (atom $ putChar 'x') (fork stop)               --> fork atom fork stop stop


-- =================================================
-- Ex. 4 - Implement (>>=) for the Concurrency Monad
-- =================================================

--  Let the types guide you
--      - There is really just one way to wire up all the pieces
--        to create a result of the required type
--      - Don't try to understand what the code does operationally,
--        trust the types

--  REMEMBER: The easiest road to implement this function is to
--      - initially ignore the Concurrent wrapper,
--          1: define a function ma >>= f
--             given ma :: ((a -> Action) -> Action)
--                   f  :: (a -> ((b -> Action) -> Action))
--             produce  :: ((b -> Action) -> Action)
--          2: let the types lead you to the only reasonable implementation
--          3: to return a value of type ((b -> Action) -> Action)
--             create something like \c -> expression-of-type-action
--          4: to pass a value of type (a -> ((b -> Action) -> Action))
--             create something like \a -> expression-of-type-((b -> Action) -> Action)
--      - later wrap and unwrap the Concurrent data type

--  NOTES:
--      - the solution only needs
--          * two lambda expressions and
--          * a couple of function applications
--      - you don't need to look at the structure of Action at all
--        (this would work for any type instead of Action)

instance Monad Concurrent where
    -- ma  :: Concurrent a
    -- f   :: a -> Concurrent b
    -- out :: Concurrent b
    (Concurrent ma) >>= f = Concurrent (\c -> ma (\a -> applyC (f a) c))
                            where applyC (Concurrent f) a = f a
    return x = Concurrent (\c -> c x)

-- action (stop >>= (\c -> stop))         --> stop
-- action (fork stop >>= \_ -> fork stop) --> fork stop fork stop stop


-- ==============================================================
-- Ex. 5 - Implement the function roundRobin :: [Action] -> IO ()
-- ==============================================================

--  At any moment, the status of the computation
--  is going to be modelled as a list of "concurrently running" actions

--  The Round Robin scheduling technique
--  (to interleave the "list of concurrently running" processes):
--      1: take the first process from the list
--      2: run its first part
--      3: take the resulting continuation
--      4: put the continuation from 3 at at the back of the list
--      5: go to 1 until the list is empty

--  NOTES:
--      - Atom monadically executes its argument and
--        puts the resulting process at the back of the process list
--      - Fork creates two new processes
--      - Stop discards its process

roundRobin        :: [Action] -> IO ()
roundRobin []     = return ()
roundRobin (a:as) = case a of
    Atom a     -> a >>= \c -> roundRobin (as ++ [c])
    Fork a1 a2 -> roundRobin (as ++ [a1] ++ [a2])
    Stop       -> roundRobin as


-- ===================================
-- Tests
-- ===================================

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))
-- run ex0 --> 183969836351184424447619541356283739

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331)
         loop $ genRandom 42
         atom (putStrLn "")
-- run ex1 --> Haskell177173719217361422167291191835716587475


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


-- ===================================
-- Suggested examples
-- ===================================

-- myex0 should produce: hohohohihihi
myex0 = run $ (ho >> ho >> ho) >>
              (hi >> hi >> hi) >> atom (putStr "\n")
    where ho = atom (putStr "ho")
          hi = atom (putStr "hi")
-- myex0 --> hohohohihihi

-- myex1 should produce: hohihohihohi
myex1 = run $ fork (ho >> ho >> ho) >>
                   (hi >> hi >> hi) >> atom (putStr "\n")
    where ho = atom (putStr "ho")
          hi = atom (putStr "hi")
-- myex1 --> hohihohihohi

-- myex2 should produce: babadibubadibudibu
myex2 = run $ fork (put3 "ba") >> fork (put3 "di") >>
        put3 "bu" >> atom (putStr "\n")
    where put3 = sequence . take 3 . repeat . atom . putStr
-- myex2 --> babadibubadibudibu

-- myex3 should produce: badibadibadi
myex3 = run $ par (put3 "ba") (put3 "di" >> stop) >>
              atom (putStr "\n")
    where put3 = sequence . take 3 . repeat . atom . putStr
-- myex3 --> badibadibadi

