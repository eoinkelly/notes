# Maybe Haskell notes

These are some random points I learned/found interesting

* A partial function is one which can't provide a valid result for all possible
  inputs.
    * they are a very common occurance in programming e.g. find a user in the database
* Haskell `Maybe` is implemented as a library - it is not part of the language
* `=` is _defining equivalence_ not variable assignment
    * This means that either side of `=` can be substituted for the other in a
      computation and you'll still get the same answer.
* most code in Haskell is in "value land" but stuff on the RHS of `::` is in "type land"
    * "type land" can be delimited by parens e.g. `(3 :: Float) / 9`
    ```hs
    almostThird = (3 :: Float) / 9 -- type signatures can be inline enclosed in parens
    ```
* Type signatures vs type annotations
    ```hs
    five :: Int -- type signature
    five = 5

    -- type annotation (note: is different from type signature)
    five = 5 :: Int
    ```


### Defining functions

```hs

-- verbose way
add :: Int -> (Int -> Int)
add = (\x -> (\y -> x + y))

-- succinct way
add :: Int -> Int -> Int
add x y = x + y
```


```
f . g = f (g x)
countEvens = length . filter even           -- works
countEvens2 xs = length (filter even xs)    -- works
countEvens3 xs = length . filter even xs    -- broken ???

countEvens5 = length . filter even -- works
countEvens5 = length (filter even) -- broken ???


fmap  :: (a -> b) -> f a -> f b
(<$>) :: (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b
(>>=) :: f a -> (a -> f b) -> f b
```

# Currying and partial application are neat in Haskell

`->` groups to the right in type definitions. This makes writing type
definitions without extra syntax to indicate that we are actually defining
muliple functions.

```hs
-- equivalent
foo :: Int -> Int -> Bool -> Bool
foo :: Int -> (Int -> (Bool -> Bool))
```

function application groups to the left. This makes passing arguments to a
curried function look natural when in fact we are passing a single arg to each
function.

```hs
-- equivalent
foo 3 5 True
(((foo 3) 5) True)
```

Together these features mean that we can have all functions be curried by
default without any unwieldy syntax.



