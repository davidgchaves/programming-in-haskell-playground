-- Exercise 12-4:
--  Define an expression fibs :: [Integer] that
--  generates the infinite sequence of Fibonacci numbers
--  using the following simple procedure:
--      1st: the first two numbers are 0 and 1;
--      2nd: the next is the sum of the previous two;
--      3rd: return to the 2nd step.
--  HINT: Use zip, tail and al list comprehension.
--  NOTE: Numbers in the Fibonacci sequence quickly become large,
--        hence the use of the type Integer of arbitrary-precision integers
fibs :: [Integer]
fibs = 0 : 1 : [x+y | (x,y) <- zip fibs (tail fibs)]
-- take 10 fibs --> [0,1,1,2,3,5,8,13,21,34]

