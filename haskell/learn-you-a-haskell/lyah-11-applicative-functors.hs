-- Haskell kata

-- Q: create a data type
-- Q: create a type class
-- Q: make an existing data type an instance of it
-- Q: make the new data type an instance of it
-- Q: make another new type that is an instance of Functor


-- TODO: Create a concrete data type that takes at least 3+ parameters
data Shape = Circle Float Float Float | 
             Rectangle Float Float Float Float 

-- TODO: Make an area function that works on all shapes
area :: Shape -> Float
area (Circle r _ _) = 2 * pi * r
area (Rectangle x1 y1 x2 y2) = (x2 - x1) * (y2 - y1)

-- TODO: Make a move function that works on all shapes
move :: Shape -> Float -> Shape
move (Circle r x y) z = Circle r (x+z) (y+z)
move (Rectangle x1 y1 x2 y2) z = Rectangle (x1 + z) (y1 + z) (x2 + z) (y2 + z)

-- TODO: Customise the show method for the type
instance Show Shape where
    show (Circle r x y) = "A circle at " ++ show x ++ "," ++ show y ++ " of radius " ++ show r
    show (Rectangle x1 y1 x2 y2) = "A rectangle from (" ++ show x1 ++ "," ++ show y1 ++ ") to (" ++ show x2 ++ "," ++ show y2 ++ ")"



data Possibly a = Probably a | Nope deriving (Show)

-- TODO: Create a type class
class Jsonable a where
  toJson :: a -> String
  fromJson :: String -> a

-- TODO: make an existing data type an instance of it
instance Jsonable Shape where
    toJson (Circle _ _ _) = "A json rep of the circle"
    toJson (Rectangle _ _ _ _) = "A json rep of a rectangle" 
    fromJson _ = Circle 1 2 3
    -- fromJson _ = if True 
    --                 then Circle 1 2 3
    --                 else Rectangle 1 2 3 4
    --
-- * when implementing a type class, it seems the arg to each function needs to be an instance of the type
-- Q: make the new data type an instance of it
-- Q: make another new type that is an instance of Functor
