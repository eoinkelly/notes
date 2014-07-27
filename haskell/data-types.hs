{-
Creating Types
==============

Here the value constructor (on RHS of =) has same _name_ as the type (both are Person)

Notes
* This seems like just a named tuple but you cannot use it interchageably with the same tuple e.g.
  a = Person "eoin" "kelly" 35 -- a value that has the 'Person' type tag attached
  b = ("eoin", "kelly", 35) -- a value that has the ([char], [char], Int) type tag attached
  are very diffrent things
* This implies that "composite" types can be different even if they contain the same fields 

Cons
  * we have to manually make reader functions (immutable data so no writers!)
  * we have to use the correct order of fields when creating values using this constructor

Pros
  * handy for small simple data types where it is obvious what each field is e.g. Point Float Float Float refers to the x, y, z coordinates.
-}
data Person = Person String String Int String deriving (Show)

-- *Main> let eoin = Person "eoin" "kelly" 35 "knocklong"
-- *Main> eoin
-- Person "eoin" "kelly" 35 "knocklong"
-- *Main> :t eoin
-- eoin :: Person

-- * Here the value constructor function (MakePaddy) has a different name to the
--   type (Irishman)
-- * when a type is shown in ghci (via implicit show) it shows the type
--   constructor function as well as the values
--
data Irishman = MakePaddy String String Int String deriving (Show)
-- *Main> let pad = MakePaddy "john" "ogorman" 33 "ballywhoo"
-- *Main> pad
-- MakePaddy "john" "ogorman" 33 "ballywhoo"
-- *Main> :t pad
-- pad :: Irishman

-- Record Syntax
-- * Advantages
--    * it automatically creates getter functions for each element
--    * You can name elements
--    * You can define them in any order when creating an instance
data Human = Human { firstName :: String,
             lastName :: String,
             age :: Int,
             address :: String } deriving (Show)
-- *Main> let mary = Human { firstName="mary", age=44, address="somewhere", lastName="blah" }
-- *Main> mary
-- Human {firstName = "mary", lastName = "blah", age = 44, address = "somewhere"}
-- *Main> :t mary
-- mary :: Human
-- *Main> age mary
-- 44
-- *Main> :t age
-- age :: Human -> Int

-- * Q: What happens if we define a function that clashes with a constructor?
-- age :: Int -> Int
-- age x = x + 1
-- * A: the module fails to compile because of multiple declarations


-- Type Constructors make new _types_ based on params
-- Value constructors make new _instances of types_ based on params
