-- http://www.youtube.com/playlist?list=PLvj5k87ljYYHwOOcdGvS3qi85IvjOW--8
--
-- module Lazycasts.Ep1
-- ( addOneTo
-- , alwaysEven
-- , funky
-- ) where

-- * notice the way the spacing and commas are done in the module statement
addOneTo :: Num a => a -> a
addOneTo i = i + 1

alwaysEven :: Integral a => a -> a -> (a, a)
alwaysEven a b = let isEven x = if even x then x else x -1
                    in (isEven a, isEven b)

-- note that the entire module is within a `where` layout block
funky :: (Integral a) => a -> a
-- funky x =
--   x + 23 + y
--   where y = 12

-- with explicit layout
funky
  x
  =
  x
  +
  23
  +
  y
  where { y = 12 }
-- notice that the function defn does _not_ start a layout block - it is all one line from the POV of the compiler
--
