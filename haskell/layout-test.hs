
-- works
foo1 :: (Num a) => a -> a -> a
foo1 x y = x + y

-- works
foo2 :: (Num a) => a -> a -> a
foo2
  x y = x + y

-- works
foo3 :: (Num a) => a -> a -> a
foo3
 x
 y
 =
 x
 +
 y

-- does not work
-- foo4 :: (Num a) => a -> a -> a
-- foo4
-- x
-- y
-- =
-- x
-- +
-- y
--

-- So it seems that the layout rules apply for functions too
