module Main(main) where

-- you actually have to have a main function when you use the explicit
-- Main.main declaration
main :: IO ()
main = putStrLn "hi there"

-- Compile this with `ghc ./testing-main.hs` and it will create a .hi and a
-- binary
