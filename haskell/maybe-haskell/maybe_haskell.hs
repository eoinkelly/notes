
five :: Int
five = 5

almostThird = 3 / 9 -- 0.3333333333333
floatyThird = (3 :: Float) / 9 -- 0.3333334
actualThird = (3 :: Rational) / 9 -- 1 % 3

add :: Int -> Int -> Int
add x y = x + y

-- these are equivalent
add 4 5
4 `add` 5

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)


-- Person on the RHS of = is a value constructor
data Person = Person String Int

pat :: Person
pat = Person "Pat" 34

getName :: Person -> String
getName (Person name _) = name

getAge :: Person -> Int
getAge (Person _ age) = age
