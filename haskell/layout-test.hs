-- QUESTION: is there an implict where claus at the top of this file?
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

-- trying whitespace at end of each line
foo4 :: (Num a) => a -> a -> a
foo4  
x  
y 
= 
x 
+ 
y 

-- foo3 :: (Num a) => a -> a -> a;
-- foo4 {
-- ;x
-- ;y
-- ;=
-- ;x
-- ;+
-- ;y
-- }

