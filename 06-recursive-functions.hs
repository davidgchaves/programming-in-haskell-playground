--
-- Exercise 1: Reimplement ^ for non-negative integers (including 0)
--
(^^^) :: Int -> Int -> Int
m ^^^ 0 = 1
m ^^^ n = m * m ^^^ (n-1)
-- 3 ^^^ 4 --> 81

(^^^^) :: Int -> Int -> Int
m ^^^^ 0 = 1
m ^^^^ n = m * (^^^^) m (n-1)
-- 3 ^^^^ 4 --> 81

