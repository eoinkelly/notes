-- Usage:
--    take 50 Fizzbuzz.fizzbuzz

module Fizzbuzz
( fizzbuzz )
where

type Strings = [String]

threes :: Strings
threes = cycle ["", "", "fizz"]

fives :: Strings
fives = cycle ["", "", "", "", "buzz"]

replacements :: Strings
replacements = zipWith (++) threes fives

digits :: Strings
digits = map show [1..]

isfb :: String -> String -> String
isfb "fizzbuzz" _ = "fizzbuzz"
isfb _ "fizzbuzz" = "fizzbuzz"
isfb "fizz" _ = "fizz"
isfb _ "fizz" = "fizz"
isfb "buzz" _ = "buzz"
isfb _ "buzz" = "buzz"
isfb _ x = x

fizzbuzz :: Strings
fizzbuzz = zipWith isfb replacements digits

