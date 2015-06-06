-- TODO: get this to work
-- import Data.Text.Lazy as T

-- Syntax summary
-- --------------
-- ::   "has type of"
-- ->   "which returns a function that returns a" 


-- Use of letter case
-- ------------------
-- * Types start with an uppercase e.g. Char, Int, Bool
-- * so functions must begin with a lowercase letter
-- * function names use camelCase

-- last type is always the return type
-- adder is a function that takes two Ints and returns an Int
-- :: read
adder :: Int -> Int -> Int

adder x y = x + y

-- haskell functions are expressions
-- => they return a value


thinger :: [Int] -> [Int] -> [Int] -- constrain thinger to [Int]
thinger :: [x] -> [x] -> [x] -- make thinger more generic (can handle any kind of list)
thinger xs ys = xs ++ ys

-- You can figure out the type signature of a function for which you have an implementation using
-- :t thinger
-- thinger :: [a] -> [a] -> [a]
-- * notice that the default type given is more premissive than the [Int] lists I specified


-- * In Haskell functions and "variable assignments" are essentially the same thing.
-- * A "variable" is just a function that always returns the same value


-- On immutability:

foo = "hi there"
-- * you can think of the above as defining a very simple function expression named foo
-- * foo takes no arguments
-- * foo always returns the same value


-- This fails to compile:
-- foo = "yo"
-- foo = "hi there"

-- This gives a parse error but works fine if I do it in GHCi
-- TODO: what is going in here?
-- let bar = "hi"
-- let bar = "yo" 

isUpper :: Char -> Bool
isUpper c = elem c ['A'..'Z']

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase str = [ c | c <- str, isUpper c]
