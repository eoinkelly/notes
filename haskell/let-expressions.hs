-- let expressions

cylinder :: Double -> Double -> Double

-- option 1: using let
-- * bindings come above function definition
-- * let is an expression - the entire let expression evaluates to a value (the result of computation after `in`)
cylinder1 r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

-- option 2: separate bindings with ; within let expression
cylinder2 r h = let sideArea = 2 * pi * r * h; topArea = pi * r ^ 2 in sideArea + 2 * topArea

-- option 2: using where
-- * bindings come after function definition (possibly nicer)
-- * where is *not* an expression - it does not evaluate to a value
cylinder3 r h = sideArea + 2 * topArea
  where sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2

