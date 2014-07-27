-- define a new type with 3 possible values
data TrafficLight = RedLight | OrangeLight | GreenLight

-- define a new typeclass that contains 2 functions
class FlowControl a where
  start :: a -> a -> Bool
  stop :: a -> a -> Bool

-- create implementations of those functions for the TrafficLight type
-- this tells haskell how
-- these functions are ...
-- what is the advantage of these type classes over just making these functions separately?
instance FlowControl TrafficLight where
  start RedLight _ = False
  start _ RedLight = False
  start _ _ = True

  stop RedLight _ = True
  stop _ RedLight = True
  stop _ _ = False

-- start RedLight RedLight      -- False
-- start GreenLight OrangeLight -- True

-- read haskell programs from right to left
