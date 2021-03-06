-- Parsers:
--  * A parser is a program that
--      - takes a string of characters, and
--      - produces some form of tree (making the syntactic structure of the string explicit)
--  * Most real-life programs use a parser to pre-process their input

import Data.Char

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

-- >>>= : apply the first parser and then the second parser
--        with the output string returned by the first parser
--        becoming the input string to the second parser
(>>>=)   :: Parser a -> (a -> Parser b) -> Parser b
p >>>= f = \inp -> case parse p inp of
                    []        -> []
                    [(v,out)] -> parse (f v) out

-- Ex1 p1c: Consumes 1 character and returns it
--          Only succeeds if every parser in its defining sequence succeeds
p1c :: Parser Char
p1c = item' `bind` (\v -> return' v)
-- p1c ""    --> []
-- p1c "a"   --> [('a',"")]
-- p1c "abc" --> [('a',"bc")]

-- Ex1 p1c: Consumes 1 character and returns it
--          Only succeeds if every parser in its defining sequence succeeds
p1c' :: Parser Char
p1c' = item' >>>= (\v -> return' v)
-- p1c' ""    --> []
-- p1c' "a"   --> [('a',"")]
-- p1c' "abc" --> [('a',"bc")]

-- Ex2 p2cs: Consumes 2 characters and returns them as a pair
--           Only succeeds if every parser in its defining sequence succeeds
p2cs :: Parser (Char, Char)
p2cs = item' `bind`
       \v1 -> item' `bind`
              \v2 -> return' (v1,v2)
-- p2cs "a"    --> []
-- p2cs "ab"   --> [(('a','b'),"")]
-- p2cs "abcd" --> [(('a','b'),"cd")]

-- Ex2 p2cs: Consumes 2 characters and returns them as a pair
--           Only succeeds if every parser in its defining sequence succeeds
p2cs' :: Parser (Char, Char)
p2cs' = item' >>>= \v1 -> item' >>>= \v2 -> return' (v1,v2)
-- p2cs' "a"    --> []
-- p2cs' "ab"   --> [(('a','b'),"")]
-- p2cs' "abcd" --> [(('a','b'),"cd")]

-- Ex3 p1st3rdAnd5thcs: Consumes 5 characters and returns the 1st, 3rd and 5th as triplet
--                      Only succeeds if every parser in its defining sequence succeeds
p1st3rdAnd5thcs :: Parser (Char, Char, Char)
p1st3rdAnd5thcs = item' `bind`
                  \v1 -> item' `bind`
                         \_ -> item' `bind`
                               \v3 -> item' `bind`
                                      \_ -> item' `bind`
                                            \v5 -> return' (v1,v3,v5)
-- p1st3rdAnd5thcs "abcd"        --> []
-- p1st3rdAnd5thcs "abcde"       --> [(('a','c','e'),"")]
-- p1st3rdAnd5thcs "abcdefghijk" --> [(('a','c','e'),"fghijk")]

-- Ex3 p1st3rdAnd5thcs: Consumes 5 characters and returns the 1st, 3rd and 5th as triplet
--                      Only succeeds if every parser in its defining sequence succeeds
p1st3rdAnd5thcs' :: Parser (Char, Char, Char)
p1st3rdAnd5thcs' = item' >>>=
                   \v1 -> item' >>>=
                          \_ -> item' >>>=
                                \v3 -> item' >>>=
                                       \_ -> item' >>>=
                                             \v5 -> return' (v1,v3,v5)
-- p1st3rdAnd5thcs' "abcd"        --> []
-- p1st3rdAnd5thcs' "abcde"       --> [(('a','c','e'),"")]
-- p1st3rdAnd5thcs' "abcdefghijk" --> [(('a','c','e'),"fghijk")]


-- 8.6 Derived Primitives
--  Using the 3 basic parsers: item, return and failure
--  Using the bind operator: >>>=

-- sat: parses single characters that satisfy the predicate p
sat   :: (Char -> Bool) -> Parser Char
sat p = item' >>>= (\v -> if p v then return' v else failure')
-- sat (== 'z') "abc" --> []
-- sat (== 'a') "abc" --> [('a',"bc")]

-- digit: parses digits
digit :: Parser Char
digit = sat isDigit
-- parse digit "ab7" --> []
-- parse digit "7ab" --> [('7',"ab")]

-- lower: parses lowercase characters
lower :: Parser Char
lower = sat isLower
-- parse lower "Abc" --> []
-- parse lower "abc" --> [('a',"bc")]

-- upper: parses uppercase characters
upper :: Parser Char
upper = sat isUpper
-- parse upper "abc" --> []
-- parse upper "Abc" --> [('A',"bc")]

-- letter: parses alphabetic Unicode characters
letter :: Parser Char
letter = sat isAlpha
-- parse letter "1bc" --> []
-- parse letter "a1b" --> [('a',"1b")]

-- alphanum: parses alphabetic and numeric digit Unicode characters
alphanum :: Parser Char
alphanum = sat isAlphaNum
-- parse alphanum "?ab" --> []
-- parse alphanum "a?b" --> [('a',"?b")]
-- parse alphanum "1?b" --> [('1',"?b")]

-- char: a char parser
char   :: Char -> Parser Char
char x = sat (== x)
-- parse (char '1') "abc" --> []
-- parse (char 'a') "abc" --> [('a',"bc")]

-- string: a string parser (only succeeds if the entire target string is consumed)
--         with the string itself returned as the result value
string        :: String -> Parser String
string []     = return' []
string (x:xs) = char x >>>= \_ -> string xs >>>= \_ -> return' (x:xs)
-- parse (string "abc") "ab123"  --> []
-- parse (string "abc") "abc123" --> [("abc","123")]

-- many:  parse 0 or more times (apply as many times as possible until it fails)
-- many1: parse 1 or more times (apply as many times as possible until it fails)
many   :: Parser a -> Parser [a]
many p = many1 p +++ return' []

many1   :: Parser a -> Parser [a]
many1 p = p >>>= \v -> many p >>>= \vs -> return' (v:vs)

-- parse (many  digit) "abc123" --> [("","abc123")]
-- parse (many1 digit) "abc123" --> []
-- parse (many  digit) "123abc" --> [("123","abc")]
-- parse (many1 digit) "123abc" --> [("123","abc")]

-- ident: parses a Haskell identifier
ident :: Parser String
ident = lower >>>= \x -> many alphanum >>>= \xs -> return' (x:xs)
-- parse ident "anIdentifier13 blah"      --> [("anIdentifier13"," blah")]
-- parse ident "     anIdentifier13 blah" --> []

-- nat: parses a Haskell natural number
nat :: Parser Int
nat = many1 digit >>>= \ns -> return' (read ns)
-- parse nat "12345 bl"    --> [(12345," bl")]
-- parse nat "   12345 bl" --> []

-- space: parses zero or more space, tab and newline characters
space :: Parser ()
space = many (sat isSpace) >>>= \_ -> return' ()
-- parse space "  \n\n   hello world 1 !" --> [((),"hello world 1 !")]


-- 8.7 Handling spacing

-- token: ignores any space before and after applying a parser for a token
token   :: Parser a -> Parser a
token p = space >>>= \_ -> p >>>= \v -> space >>>= \_ -> return' v
-- parse (token nat) "    \n   123456\n  \n" --> [(123456,"")]

-- identifier: parser for Haskell identifiers
identifier :: Parser String
identifier = token ident
-- parse identifier "  \n aNewIdentifier24\n  " --> [("aNewIdentifier24","")]

-- natural: parser for Haskell natural numbers
natural :: Parser Int
natural = token nat
-- parse natural "\n\n  123456  \n " --> [(123456,"")]

-- symbol: parser for Haskell special symbols (like [])
symbol    :: String -> Parser String
symbol xs = token (string xs)
-- parse (symbol "[") "\n  [123]  " --> [("[","123]  ")]

-- p: a parser for a non-empty list of natural numbers that ignores spacing around tokens
p :: Parser [Int]
p = symbol "[" >>>=
    \_  -> natural >>>=
    \n  -> many (symbol "," >>>= \_ -> natural) >>>=
    \ns -> symbol "]" >>>=
    \_  -> return' (n:ns)
-- parse p " \n  [1, 2, 3,4,5, 6]  \n" --> [([1,2,3,4,5,6],"")]
-- parse p "  [1,2, ] "                --> []


-- 8.8 Aritmethic expressions
--  A Grammar for a simple form of arithmetic expressions:
--      - built up from natural numbers
--      - using addition, multiplication, and parentheses
--      - addition and multiplication associate to the right
--      - multiplication has higher priority than addition
--  Examples:
--      - 2 + 3 + 4 means 2 + (3 + 4)
--      - 2 * 3 + 4 means (2 * 3) + 4
--  Final Grammar:
--      expr   ::= term (+ expr | E)
--      term   ::= factor (* term | E)
--      factor ::= (expr) | nat
--      nat    ::= 0 | 1 | 2 | ...

expr :: Parser Int
expr = (term >>>= plusExpr) +++ (term >>>= epsilon)
       where
           plusExpr = \t -> symbol "+" >>>= \_ -> expr >>>= \e -> return' (t+e)
           epsilon  = \t -> return' t

term :: Parser Int
term = (factor >>>= multiTerm) +++ (factor >>>= epsilon)
       where
           multiTerm = \f -> symbol "*" >>>= \_ -> term >>>= \t -> return' (f*t)
           epsilon   = \f -> return' f

factor :: Parser Int
factor = exprWithParens +++ natural
         where
             exprWithParens = symbol "(" >>>= \_ -> expr >>>= \e -> symbol ")" >>>= \_ -> return' e

-- parse expr "2*3+4"         --> [(10,"")]
-- parse expr "2*(3+4)"       --> [(14,"")]
-- parse expr "2 * ( 3 + 4 )" --> [(14,"")]

eval    :: String -> Int
eval xs = case parse expr xs of
            [(n,[])]  -> n
            [(_,out)] -> error ("unused input" ++ out)
            []        -> error "invalid input"
-- eval "2*3+4" --> 10
-- eval "2*3-4" --> *** Exception: unused input-4
-- eval "-1"    --> *** Exception: invalid input

