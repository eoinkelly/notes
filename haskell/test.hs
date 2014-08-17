
-- this works because function application has the highest precedence
doubleUs x y = doubleMe x + doubleMe y

-- it does not seem to matter what order functions are defined in the source
doubleMe x = x + x

doubleSmallNum x = if x > 100
                    then x
                    else x * 2

doubleSmallNum' x = (if x > 100 then x else x * 2)
doublePlus1 x = (if x > 100 then x else x * 2) + 1

-- this is defining a function with 0 args. wild.
-- so xxx is a label for a value that happens to be a string
xxx = "foo foo foo"
let yyy = "foo foo foo"

-- and doubleMeAgain is a label for a value that happens to be executable code
doubleMeAgain x = x + x


-- a tuple is an list
-- of a particular length
-- containing particular types
-- in a particular order
--
-- a tuple is defined by its
-- * length
-- * types
-- * order that the types are in
--
-- "that is a tuple of length 4 which is 2 integers followed by a float and then a character"
-- (2,3,5.6,'a')
--
-- it is a sort of strict data object
-- ("chuck", "norris", 55, "actor")
-- you can rely on the first element of a tuple always being the same type
