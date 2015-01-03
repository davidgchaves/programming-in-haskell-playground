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
