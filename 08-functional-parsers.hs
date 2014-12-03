-- Parsers:
--  * A parser is a program that
--      - takes a string of characters, and
--      - produces some form of tree (making the syntactic structure of the string explicit)
--  * Most real-life programs use a parser to pre-process their input


-- 8.2 The Parser type

-- Evolution of the Parser type
--  * The notion of a parser can be represented as a function of
--      type Parser = String -> Tree
--
--  * A parser might not always consume its entire argument string,
--    (return any unconsumed part of the argument string):
--      type Parser = String -> (Tree, String)
--
--  * A parser might not always succeed or might have multiple results:
--      - The empty list            denotes failure
--      - A singleton list          denotes success
--      - More than one pair list   denotes ambiguous grammar
--        (ambiguous grammar: the argument string can be parsed in more than one way)
--      NOTE: For simplicity we only consider parsers that return at most one result
--      type Parser = String -> [(Tree, String)]
--
--  * Different parsers will likely return different kinds of trees,
--    or more generally, any kind of value a (parameter of the Parser type)
type Parser a = String -> [(a, String)]


-- 8.3 Basic Parser - building blocks for all other parsers

-- return: the parser that always succeeds
return'   :: a -> Parser a
return' v = \inp -> [(v,inp)]
-- parse (return' 1) "abc" --> [(1,"abc")]

-- failure: the parser that always fails
failure' :: Parser a
failure' = \inp -> []
-- parse failure' "abc" --> []

-- item: try to parse the first Char of the String
item' :: Parser Char
item' = \inp -> case inp of
                    []     -> []
                    (x:xs) -> [(x,xs)]
-- parse item' ""    --> []
-- parse item' "abc" --> [('a',"bc")]

-- parse: applies a Parser to a String
parse       :: Parser a -> String -> [(a,String)]
parse p inp = p inp


-- 8.5 Choice - combining two parsers (+++ AKA orElse)

-- +++: apply the first parser and if it fails, apply the second parser
(+++)   :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case parse p inp of
                    []        -> parse q inp
                    [(v,out)] -> [(v,out)]
-- parse (return' 'A' +++ return' 'd') "abc" --> [('A',"abc")]
-- parse (failure'    +++ return' 'd') "abc" --> [('d',"abc")]
-- parse (item'       +++ return' 'd') "abc" --> [('a',"bc")]

-- orElse: apply the first parser and if it fails, apply the second parser
orElse       :: Parser a -> Parser a -> Parser a
p `orElse` q = \inp -> case parse p inp of
                        []        -> parse q inp
                        [(v,out)] -> [(v,out)]
-- parse (return' 'A' `orElse` return' 'd') "abc" --> [('A',"abc")]
-- parse (failure'    `orElse` return' 'd') "abc" --> [('d',"abc")]
-- parse (item'       `orElse` return' 'd') "abc" --> [('a',"bc")]


-- 8.4 Sequencing - combining sequentally two parsers (bind AKA then AKA >>=)

-- bind: apply the first parser and then the second parser
--       with the output string returned by the first parser
--       becoming the input string to the second parser
bind       :: Parser a -> (a -> Parser b) -> Parser b
p `bind` f = \inp -> case parse p inp of
                        []        -> []
                        [(v,out)] -> parse (f v) out

-- >>= : apply the first parser and then the second parser
--       with the output string returned by the first parser
--       becoming the input string to the second parser
(>>=)   :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \inp -> case parse p inp of
                    []        -> []
                    [(v,out)] -> parse (f v) out

-- Ex1 p1c: Consumes 1 character and returns it
--          Only succeeds if every parser in its defining sequence succeeds
p1c :: Parser Char
p1c = item' `bind` (\v -> return' v)
-- p1c ""    --> []
-- p1c "a"   --> [('a',"")]
-- p1c "abc" --> [('a',"bc")]

