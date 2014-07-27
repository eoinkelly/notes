-- haskell whtespace rules are on the following blocks
-- do
-- where
-- in
-- if
-- else

-- main has the type of the IO of unit (the empty tuple)
-- * this type signature will work provided that our final IO action in the do block returns `()` - many IO actions 
--   do this by default e.g. putStr and friends
main :: IO ()
-- main = putStrLn "hello there"

mult :: (Num x) => x -> x
mult x = x * x

-- even if I create helper functions that do IO they will only run when run from main
-- this means that all IO will live on the call stack that begins at main
main = do
  putStrLn "hello what is your name?"
  name <- getLine
  aa <- return $ show $ mult 3
  putStrLn ("Hey " ++ name ++ aa ++ " you rock!") -- this is the last expression in the do block so it is what is returned
  -- xxx <- foo
  let xxx = 12 :: Int
  putStrLn $ "hi" ++ (show xxx)
  _ <- return "hi"
  return ()

-- `do` creates a single IO action, the type of which is decided by the type of the final IO action within the do block
--    * it is a composer of IO actions
--    * it is a maker of composite IO actions which themselves contain only IO actions 
-- * because we do not usually know what type of IO action we will return in the last line, we usually don't specify a type for main
-- * it seems like `do` blocks can *only* contain expressions that return an IO action
-- * `do` can chain any actions as long as al of them are in the same monad
-- * it is syntax sugar for the `>>` and `>>=` monad operators 
-- * ultimately do syntax is just sugar and you don't have to use it.
-- * the last IO action in a do block _cannot_ be bound to a name

-- the order of statements does not determine the order of execution
foo :: Maybe Int
foo = do x <- Just (3 + 5)
         y <- Just (9 * 3)
         return (x-y)
-- the calculation of x & y could happen in either order (or in parallel)
-- in general if you ignore the result of a function in haskell, it will not even be evaluated



-- `<-` is a thing that can extract the returned value from an IO action
--   * it only works within another IO action i.e. within a `do` block


-- return
-- * is not a final statement that changes control flow (as it is in C)
-- * it creates a new IO action and wraps whatever param you give it.
-- :t return
-- return :: Monad m => a -> m a
-- * it takes a type a and returns a monad of a
-- * I guess it is kind of a "wrap a value in a monad" function

-- what are the consequences of only being able to get input from within another IO action?
-- * does it mean you can only have on IO action section in your program?
-- * every IO action that is performed (read "is under main") produces a result
