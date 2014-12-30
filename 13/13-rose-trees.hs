------------------------------------------------------------------------------------------------------------------------------
-- ROSE TREES, FUNCTORS, MONOIDS, FOLDABLES
------------------------------------------------------------------------------------------------------------------------------

-- A Rose Tree (multi-way tree) is a tree data structure in which each node can
--  - store one value and
--  - have an arbitrary number of children
data Rose a = a :> [Rose a] deriving Show

-- ===================================
-- Ex. 0-2
-- ===================================

root          :: Rose a -> a
root (a :> _) = a
-- root (1 :> [2 :> [], 3 :> []]) --> 1
-- root ('a' :> [])               --> 'a'

children           :: Rose a -> [Rose a]
children (_ :> as) = as
-- children (1 :> [2 :> [], 3 :> []]) --> [2 :> [],3 :> []]
-- children ('a' :> [])               --> []

tree1 = 'x' :> map (flip (:>) []) ['a'..'x']
-- length $ children tree1 --> 24

tree2 = 'x' :> map (\c -> c :> []) ['a'..'A']
-- length (children tree2) --> 0

xs = 0 :> [1 :> [2 :> [3 :> [4 :> [], 5 :> []]]], 6 :> [], 7 :> [8 :> [9 :> [10 :> []], 11 :> []], 12 :> [13 :> []]]]

ex2 = root . head . children . head . children . head . drop 2 $ children xs
-- ex2 --> 9

-- ===================================
-- Ex. 3-7
-- ===================================

size           :: Rose a -> Int
size (_ :> []) = 1
size (_ :> as) = 1 + sum (map size as)

leaves :: Rose a -> Int
leaves (_ :> []) = 1
leaves (_ :> as) = sum (map leaves as)

xs1 = 0 :> [1 :> [2 :> [3 :> [4 :> []]]]]
-- size xs1   --> 5
-- leaves xs1 --> 1

xs2 = 0 :> [1 :> [2 :> [3 :> [4 :> [], 5 :> []]]]]
-- size xs2   --> 6
-- leaves xs2 --> 2

tree3 = 1 :> map (\c -> c :> []) [1..5]
-- size tree3                            --> 6
-- size . head . children $ tree3        --> 1
-- leaves tree3                          --> 5
-- product (map leaves (children tree3)) --> 1

ex7 = (*) (leaves . head . children . head . children $ xs) (product . map size . children . head . drop 2 . children $ xs)
-- ex7 --> 16

-- ===================================
-- Ex. 8-10
-- ===================================

-- Similarly to how we might want to apply a function uniformly to all elements in a list,
-- we might also want to apply a function uniformly to all the elements in a rose tree,
-- or any other container-like data structure for that matter.
--
-- For this purpose Haskell has a Functor type class,
-- exposing a single function fmap that generalizes the map function:
--
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

instance Functor Rose where
--  fmap             :: (a -> b) -> Rose a -> Rose b
    fmap f (a :> as) = f a :> (map (fmap f) as)
-- fmap (*2) (1 :> [2 :> [], 3 :> []]) --> 2 :> [4 :> [],6 :> []]
-- fmap (+1) (1 :> [])                 --> 2 :> []

-- size (fmap leaves (fmap (:> []) tree3)) --> 6

-- Exercise 9 (playing with types)
f1   :: Rose a -> Rose a
f1 r = fmap head $ fmap (\x -> [x]) r

ex10 = round . root . head . children . fmap (\x -> if x > 0.5 then x else 0) $ fmap (\x -> sin(fromIntegral x)) xs
-- ex10 --> 1

-- ===================================
-- Ex. 11-13
-- ===================================

-- A Monoid is an algebraic structure over a type m with
--  - a single associative binary operation mappend :: m -> m -> m
--  - an identity element mempty :: m

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m

-- Numbers form a Monoid, both
--  - under addition with 0 as the identity element
--  - under multiplication with 1 as the identity element

-- We are only allowed to give one instance per combination of type and type class.
-- To overcome this limitation we create some newtype wrappers:
newtype Sum a = Sum a
newtype Product a = Product a

unSum         :: Sum a -> a
unSum (Sum a) = a
-- unSum $ Sum 5 --> 5

instance Num a => Monoid (Sum a) where
    mempty                    = Sum 0
    (Sum x) `mappend` (Sum y) = Sum (x+y)
-- unSum $ Sum 4 `mappend` mempty --> 4
-- unSum $ Sum 4 `mappend` Sum 3  --> 7

unProduct             :: Product a -> a
unProduct (Product a) = a
-- unProduct $ Product 6 --> 6

instance Num a => Monoid (Product a) where
    mempty                            = Product 1
    (Product x) `mappend` (Product y) = Product (x*y)
-- unProduct $ mempty    `mappend` Product 5 --> 5
-- unProduct $ Product 4 `mappend` Product 5 --> 20

-- unProduct (Product 6 `mappend` (Product . unSum $ Sum 3 `mappend` Sum 4)) --> 42

num1 = mappend (mappend (Sum 2) (mappend (mappend mempty (Sum 1)) mempty)) (mappend (Sum 2) (Sum 1))

num2 = mappend (Sum 3) (mappend mempty (mappend (mappend (mappend (Sum 2) mempty) (Sum (-1))) (Sum 3)))

ex13 = unSum (mappend (Sum 5) (Sum (unProduct (mappend (Product (unSum num2)) (mappend (Product (unSum num1)) (mappend mempty (mappend (Product 2) (Product 3))))))))
-- ex13 --> 257

-- ===================================
-- Ex. 14-15
-- ===================================

-- If f is some container-like data structure storing elements of type m that form a Monoid,
-- then there is a way of folding all the elements in the data structure
-- into a single element of the monoid m.

-- The following declaration defines the type class Foldable:
class Functor f => Foldable f where
    fold :: Monoid m => f m -> m

    foldMap   :: Monoid m => (a -> m) -> (f a -> m)
    foldMap f a = fold $ fmap f a

instance Foldable Rose where
    fold (a :> as) = a `mappend` (foldr mappend mempty (map fold as))

tree4 :: Rose Int
tree4  = 1 :> [2 :> [], 3 :> [4 :> []]]

tree5 :: Rose (Product Int)
tree5 = fmap Product tree4
-- unProduct $ fold tree5 --> 24

sumxs = Sum 0 :> [Sum 13 :> [Sum 26 :> [Sum (-31) :> [Sum (-45) :> [], Sum 23 :> []]]], Sum 27 :> [], Sum 9 :> [Sum 15 :> [Sum 3 :> [Sum (-113) :> []], Sum 1 :> []], Sum 71 :> [Sum 55 :> []]]]

ex15 = unSum (mappend (mappend (fold sumxs) (mappend (fold . head . drop 2 . children $ sumxs) (Sum 30))) (fold . head . children $ sumxs))
-- ex15 --> 111

-- ===================================
-- Ex. 16-18
-- ===================================

tree6 = 42 :> [3 :> [2:> [], 1 :> [0 :> []]]]
-- unSum $ foldMap Sum tree6 --> 48

ex17 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (mappend (foldMap (\x -> Sum x) . head . drop 2 . children $ xs) (Sum 30))) (foldMap (\x -> Sum x) . head . children $ xs))
-- ex17 --> 206

ex18 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (Sum (unProduct (mappend (foldMap (\x -> Product x) . head . drop 2 . children $ xs) (Product 3))))) (foldMap (\x -> Sum x) . head . children $ xs))
-- ex18 --> 25946026

-- ===================================
-- Ex. 19-21
-- ===================================

fproduct, fsum :: (Foldable f, Num a) => f a -> a
fsum     xs    = unSum     $ fold $ fmap Sum xs
fproduct xs    = unProduct $ fold $ fmap Product xs
-- fsum xs     --> 91
-- fproduct xs --> 0

ex21 = ((fsum . head . drop 1 . children $ xs) + (fproduct . head . children . head . children . head . drop 2 . children $ xs)) - (fsum . head . children . head . children $ xs)
-- ex21 --> 82

