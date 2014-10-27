--
-- Exercise 3: Correct the syntatic errors
--
n = a `div` length xs
    where a = 10
          xs = [1,2,3,4,5]

--
-- Exercise 4: Reimplement 'last'
--
ns = [1,2,3,4,5]
cs = "aeiou"

last'   xs = head (drop (length xs - 1) xs)
last''  xs = head (reverse xs)
last''' xs = xs !! (length xs - 1)
-- last ns --> 5
-- last cs --> 'u'

--
-- Exercise 5: Reimplement 'init'
--
init'  xs = take (length xs - 1) xs
init'' xs = reverse (tail (reverse xs))
-- init ns --> [1,2,3,4]
-- init cs --> "aeio"

