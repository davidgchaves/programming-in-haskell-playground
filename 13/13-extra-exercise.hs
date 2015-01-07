--
-- Extra Exercise: Implement foldl in terms of foldr
--

--  1 - The Universal Property of foldr
--
--    The Universal Property of foldr says that if we have some function g defined as
--      g []     = v            (BASE CASE)
--      g (x:xs) = f x (g xs)   (RECURSIVE CASE)
--    then
--      g = foldr f v
--
--    If we substitute 'foldr f v' into the definition of g we get a definition of foldr:
--      foldr f v []     = v
--      foldr f v (x:xs) = f x (foldr f v xs)
--
--
--  2 - Comparing foldl and foldr
--
--    The base case of foldr and foldl is identical
--      foldr f v [] = v
--      foldl f v [] = v
--
--    The recursive one is not
--      foldr f v (x:xs) = f x (foldr f v xs)
--      foldl f v (x:xs) = foldl f (f v x) xs
--
--
--  3 - Transforming the foldl definition
--
--    We are going to apply a transformation to the foldl definition
--    so it can be rewritten in the form
--      f x (g xs)
--
--    3.1 - The foldl definition with types
--      foldl            :: (a -> b -> a) -> a -> [b] -> a
--      foldl f a []     = a
--      foldl f a (b:bs) = foldl f (f a b) bs
--
--    3.2 - Swapping the last 2 parameters - enter foldl2
--      foldl2 f []     a = a
--      foldl2 f (b:bs) a = foldl2 f bs (f a b)
--
--    3.3 - Rewriting foldl2 base case using id and an eta-conversion
--      id: returns the input parameter unchanged (identity function)
--      foldl2 f [] a = a  --->  foldl2 f [] a = id a
--
--      eta-conversion: adding or dropping of abstraction over a function
--      foldl2 f [] a = id a  --->  foldl2 f [] = id
--
--    3.4 - Rewriting foldl2 recursive case using a lambda and an eta-conversion
--      We use a lambda to factor out the value a
--      foldl2 f (b:bs) a = (\a -> foldl2 f bs (f a b)) a
--
--      And now the eta-conversion
--      foldl2 f (b:bs) = \a -> foldl2 f bs (f a b)
--
--    3.5 - Factoring out b in foldl2 recursive case
--      We use the previous lambda to factor out the list element b
--      foldl2 f (b:bs) = (\b a -> foldl2 f bs (f a b)) b
--
--    3.6 - Factoring out 'foldl2 f bs' in foldl2 recursive case
--      And yet again, we use the previous lambda to factor out 'foldl2 f bs' (as h)
--      foldl2 f (b:bs) = (\b h a -> h (f a b)) b (foldl2 f bs)
--
--
--  4 - foldl2: the new foldl definition
--
--    This is how foldl2 is defined after all the transformations
--      foldl2 f []     = id
--      foldl2 f (b:bs) = (\b h a -> h (f a b)) b (foldl2 f bs)
--
--
--  5 - foldl2 and the "g-form" of the Universal Property of foldr
--
--    Right now we have almost managed to transform foldl
--      foldl2 f (b:bs) = (\b h a -> h (f a b)) b (foldl2 f bs)
--    into the "g-form" of the Universal Property of foldr
--      g (x:xs) = k x (g xs)
--    or more conveniently
--      g (b:bs) = k b (g bs)
--
--    Just check it out
--      g        (b:bs) = k                     b (g        bs)
--      foldl2 f (b:bs) = (\b h a -> h (f a b)) b (foldl2 f bs)
--
--    where
--      k = \b h a -> h (f a b)
--      g = foldl2 f
--
--
--  6 - Combining foldl2 and foldr using the Universal Property of foldr
--
--    Consider the Universal Property of foldr (from 1)
--      g = foldr k v
--    or more conveniently
--      g = foldr k a
--
--    and the transformed foldl (from 5)
--      g = foldl2 f
--
--    Combining both, we arrive at
--      foldl2 f = foldr k a
--
--
--  7 - foldl2 in terms of foldr
--
--    We are going to replace 'k' and 'a' in
--      foldl2 f = foldr k a
--    and then restore the removed parameters 'bs' and 'a'
--
--    7.1 - Replacing 'a' for 'id'
--      We know from the base case (from 3.3) that
--      a = id
--      so (from 6)
--      foldl2 f = foldr k id
--
--    7.2 - Replacing 'k' for the lambda expression
--      We know (from 5) that
--      k = \b h a -> h (f a b)
--      so
--      foldl2 f = foldr (\b h a -> h (f a b)) id
--
--    7.3 - Restoring the parameters 'bs' and 'a' removed with eta-reductions
--      We could restore the two parameters 'bs' and 'a' (from 3)
--      that were removed by eta-reductions
--      foldl2 f bs a = foldr (\b h a -> h (f a b)) id bs a
--
--
--  8 - foldl in terms of foldr
--
--    Remember that foldl2 was only a rework of foldl (from 3.2)
--    If we go back to the original foldl then
--    we can finally express foldl in terms of foldr
--
--      foldl f a bs = foldr (\b h a -> h (f a b)) id bs a

foldl'        :: (a -> b -> a) -> a -> [b] -> a
foldl' f a bs = foldr (\b h a -> h (f a b)) id bs a
-- foldl' (+) 0 [1..5] --> 15

--  9 - Checking the type correction of our solution
--
--    As a reference this is the foldr type
--      :t foldr --> foldr :: (a -> b -> b) -> b -> [a] -> b
--
--    and this the foldl type
--      :t foldl --> foldl :: (a -> b -> a) -> a -> [b] -> a
--
--    9.1 - The first parameter to foldr
--      The first parameter to foldr, it's the cryptic lambda
--      (\b h a -> h (f a b))
--      which uses the f function that comes as the first argument to foldl
--
--    9.2 - The simplest possible f (as in foldl f a bs)
--      So f type should be
--      :t f --> f :: (a -> b -> a)
--
--      The simplest possible f could be
--      let f = (\a b -> a)
--
--    9.3 - The lambda type of the first parameter to foldr
--      Using the previous f, we could know the lambda type
--      :t (\b h a -> h (f a b))
--        --> (\b h a -> h (f a b)) :: t2 -> (t1 -> t) -> (t1 -> t)
--
--      And this correspond to the expected type for the 1st argument to foldr
--      (a -> b -> b)
--
--      We just need to make this match
--      't2'        <--->  'a'
--      '(t1 -> t)' <--->  'b'
--
--    9.4 - The new type signature of foldr
--      Remember the foldr type (from 9)
--      :t foldr --> foldr :: (a -> b -> b) -> b -> [a] -> b
--
--      Taking into account the new matches (from 9.3),
--      our new foldr type could be written as
--      :t foldr --> foldr :: (t2 -> (t1 -> t) -> (t1 -> t)) -> (t1 -> t) -> [t2] -> (t1 -> t)
--
--      Side by side:
--      foldr :: (a  -> b         -> b)         -> b         -> [a]  -> b
--      foldr :: (t2 -> (t1 -> t) -> (t1 -> t)) -> (t1 -> t) -> [t2] -> (t1 -> t)
--
--    9.5 - foldr has now 4 parameters
--      If we drop the parentheses in the foldr type (from 9.4)
--      foldr :: (t2 -> (t1 -> t) -> (t1 -> t)) -> (t1 -> t) -> [t2] -> t1 -> t
--
--      we arrive at a foldr type with 4 parameters
--
--    9.6 - The 4 parameters of foldr
--      1st parameter: f :: (t2 -> (t1 -> t) -> (t1 -> t))
--      2nd parameter: ? :: (t1 -> t)
--      3rd parameter: ? :: [t2]
--      4rd parameter: ? :: t1
--      producing:     ? :: t
--
--    9.7 - About the id function
--      According to our implementation
--      foldl f a bs = foldr (\b h a -> h (f a b)) id bs a
--      the id function is the second parameter to foldr
--
--      Taking into account that the id function type is
--      :t id --> id :: a -> a
--
--      we need to consider an additional restriction in
--      2nd parameter: ? :: (t -> t)
--      4rd parameter: ? :: t
--
--    9.8 - Type correction of the 4-parameter-foldr
--      This is the final foldr type with the additional restriction imposed by id function
--      foldr :: (t2 -> (t -> t) -> (t -> t)) -> (t -> t) -> [t2] -> t -> t
--
--    9.9 - Disecting the 4 parameters of foldr
--      1st parameter: f  :: (t2 -> (t -> t) -> (t -> t))
--      2nd parameter: id :: (t -> t)
--      3rd parameter: bs :: [t2]   (list we are folding)
--      4rd parameter: a  :: t      (initial accumulator value)
--      producing:     a  :: t      (final result)

--  FOOD FOR THOUGHT
--  (1) "A tutorial on the universality and expressiveness of fold" by Graham Hutton
--      http://www.cs.nott.ac.uk/~gmh/fold.pdf
--  (2) "Expressing foldl in terms of foldr" by Jan Stolarek
--      http://lambda.jstolarek.com/2012/07/expressing-foldl-in-terms-of-foldr/
--  (3) "Haskell foldl as foldr"
--      http://blog.wakatta.jp/blog/2011/11/09/haskell-foldl-as-foldr/

