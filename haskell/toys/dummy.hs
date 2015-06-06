-- Introduction to Haskell
-- -----------------------

-- When invoking a function, just write its name and args separated by spaces
-- function application has the highest precedence so these 2 lines are equivalent
-- succ 9 + max 5 4 + 1
-- (succ 9) + (max 5 4) + 1

-- NOTE: brackets are just used to control evaluation order!
-- bar (bar 3)

-- Functions with special character names are assumed to be infix - surround
-- them with () to use as prefix.

-- 4 + 3 
-- (+) 4 3 
-- this is the opposite of backticks which let you use a prefix func with 2 args as an infix
-- div 10 2
-- 10 `div` 2

--Function invocation:
-- {func-name} {optional list of params} = {function body}


-- doubleMe :: Int -> Int -- works
-- doubleMe :: a -> a -- fails
doubleMe :: Num a => a -> a -- works
doubleMe x = x + x

-- * functions can be declared out order to how they are used e.g. doubleUs
-- * could be defined before doubleMe in the source

doubleUs :: Int -> Int -> Int
doubleUs x y = doubleMe x + doubleMe y

-- * When defining a function start with the most specific cases and end with
--   the most general.
sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe _ = "No!!!" -- the last pattern is the "catch all" pattern

-- * Order matters when defining function cases - if these cases were in
--   reverse the function would never end.
factorial :: Integral a => a -> a
factorial 0 = 1
factorial x = x * factorial (x - 1) -- the "catch all" pattern


-- * functions cannot start with a capital letter
-- * functions that end in ' e.g. foo' is a convention that denotes either
-- 1. a strict (not lazy) version of a function
-- 2. a slightly modified version of a function
--  functions can have ' in the middle of their name e.g. johnO'Grotes x = x + 10

-- When a function doesn't take any parameters we call it a "definition" or a
-- "name" i.e. its a variable assignment

-- * In functional languages a variable name is a label|name for a value which
--   cannot be changed.
-- * In imperitive languages a variable name is a label for a memory-location,
--   the contents of which can change.


-- head2 :: [e] -> e
-- head2 [] = error "Cannot do head thing on an empty list!"
-- head2 (x:_) = x -- if we bind to more than one variable, we need to surround them in ()

-- exactly the same as above minus the syntatic sugar
head2 :: [e] -> e
head2 x = case x of []    -> error "Cannot do thead thing on an empty list!"
                    (x:_) -> x


describeList :: [a] -> String
describeList xs = "This list is " ++ case xs of []  -> "empty"
                                                [x] -> "short"
                                                xs  -> "long"

capital :: String -> String
capital "" = error "Cannot get first letter of an empty string"
capital all@(x:xs) = "first letter of '" ++ all ++ "' is:" ++ [x]

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.5 = "you thin"
  | bmi <= 25 = "you medium"
  | bmi <= 30 = "you medium-big"
  | otherwise   = "you real big!" -- the catch-all case
-- so we have made different versions of the function depending on the numeric value of the pararmeter
-- function bmiTell(bmi)
--    if bmi <= 18.5
--      return "you thin"
--    ...
--    else
--      return "you real big!"

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering

-- implement as a func that takes one a param and returns an a
-- compareWithHundred x = compare 100 x 

-- implement as a partially applied function. the arg will be appli
compareWithHundred = compare 100 

-- works the same either way:
-- compareWithHundred 10

