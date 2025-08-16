# Chapter 1

## Representing negative numbers

Represent negative numbers in computations by wrapping them in `()` so that the
parser does not get confused:

```haskell
ghci> 3 + (-4)

-- haskell thinks you have put two infix operators beside each other here
ghci> 3 + -4
```

- Comparison operators work as in other languages except they are strictly typed
  (so you can only compare things of the same type).

The Haskell comparison operators:

```
&&
||
not
==
/=
```

Haskell seems to cheat a bit when combining numberic types (or maybe I don't
understand the type system properly yet)

```
ghci> :t 4
4 :: Num t => t
ghci> :t 4.5
4.5 :: Fractional t => t
ghci> 4 + 4.5
8.5

ghci> :t 4.5
4.5 :: Fractional t => t
ghci> :k Fractional
Fractional :: * -> Constraint
ghci> :t (+)
(+) :: Num a => a -> a -> a
```

### Calling functions (applying functions)

- whitespace is used to apply functions in haskell
- functions can either be called in the _prefix_ way or the _infix_ way
- functions named with punctuation (operators) default to infix, all others
  default to prefix
- to call an operator in prefix way wrap the operator in parentheses
    - `(+) 4 6`, `(*) 4 6`
    - Note you do not wrap the whole function application in parens like lisps
      do
- you can call a function that is normally _prefix_ as an _infix_ provided that

1. it is a binary function (takes wo args)
    - we convert it by wrapping it in backticks `15 `div` 5`

```
ghci> succ 8
9
ghci> max 3 56
56
ghci> 3 `max` 56
56
ghci> let eoindo x y = x + y
ghci> 4 `eoindo` 5
9
```

- function application is kind of invisible when viewing haskell code - it is
  everywhere in the whitespace
- it has the highest precedence of any operation in haskell (precedence 10
  (CHECK THIS))
- it groups to the left (left associative) so

    ```haskell
    succ succ succ 8
    -- fails because it becomes
    (((succ succ) succ) 8)
    -- we can fix it by doing
    succ (succ (succ 8))
    -- or
    succ $ succ $ succ 8
    ```

    - this means when haskell evaluates an expression it does all the function
    - application first

- creating functions and creating named data are the same thing in haskell
- a pure function that needs some args to compute its value is just a fancy
  piece of data

a named piece of data is just a function that returns the same thing every time
you can think of it as a "data source" - a function that can pump data into the
data transformation pipelines we setup

- functions (which are also data) in haskell don't have to be defined in your
  source files before you use them.

We can _name_ data in Haskell

The haskell toolbox does _not_ contain named slots in memory that we can change
during the lifetime of the program

### if else

- is an expression in haskell
    - so it always returns a value
    - so the `else` part is required

### Naming conventions for strict (or modified) functions

- Use `'` as suffix e.g. `succ'` alerts the user that this is not the built-in
  `succ` and might behave differently.

### Strictness & Laziness

Most implementations of haskell run code in a _lazy_ way. This means that when
the compiler finds a function application in the source code it doesn't perform
it, it instead makes a note of it and will perform it later if some other code
requires it. This note taking has performance impacts.

laziness = non strict + sharing

Sometimes laziness is a performance win e.g. an infinite list but it does have
overhead.

To be lazy the compiler can't evaluate function applications when it finds them
in source code. Instead it records them in memory (on the heap) (in a _thunk_ or
_suspension_) that _can_ be evaluated later. This is unnecessary overhead if the
expression will definitely be evaluated.

It also means that every function that is evaluated needs to access the heap
which makes evaluation slower than it would be if the function application was
just evaluated when it was found.

- GHC tries to figure out which functions are definitely called (called
  _strictness analysis_ so that it can evaluate them immediately (rather than
  store & evaluate)
- There are limits to how good strictness analysis is

How do we explicitly make things strict?

1. Use the strict version of a function if it is available e.g. `foldl'` vs
   `foldl`
2. Use the _BangPatterns_ GHC extension
    ```haskell
    -- GHC might create suspensions for xs, x, y
    somefunc xs x y = ...
    -- tell GHC that x and y should be evaulated immediately (no suspensions)
    somefunc xs !x !y =
    ```
3. Use the strict variant of the infix application operator `!$`
    ```haskell
    f (g x)
    -- this form is better if
    -- 1. you are definitely going to evaluate g x anyway
    -- 2. f is not obviously stict (so GHC won't figure it out)
    f !$ g x
    ```

### it

- contains the result of the last command in `ghci` - similar to `_` in IRB

### Scope in Haskell

- Haskell code is broken up into modules
- Modules control what identifiers they export
- A consuming module can alias the names exported if it wants to
- so there are "scope lines" around modules
- The top level module is Main. By default it only exports one function (`main`)

QUESTION: what about scope within a module? things that seem to ahve their own
scope: functions let expressions if-else

## Program entry point is Main.main

- Haskell 98 specification says that the entry point of a program, namely,
  function main, should reside in the module called Main, by convention.

- If you don't explicitly include the header, the compiler will add it
    > An abbreviated form of module, consisting only of the module body, is
    > permitted. If this is used, the header is assumed to be "module Main(main)
    > where".

```haskell
-- if this header is missing, haskell will add it (just as written below)
-- if you want to change what Main exports you need to be explicit
module Main(main) where

-- the explicity declaration above said we were exporting main so we need a main function!
main :: IO ()
main = ...
```
