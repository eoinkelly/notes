
# Empty things

* there is no null or void type
* uses `()` as the convertion to denote something empty
* the type of `()` is `()`
    ```
    :t ()
    () :: ()
    ```
* `undefined` is used to represent some sort of error state - it is not something
  to expect as return value etc.
    ```
    :t undefined
    undefined ::
    ```
    * it is a polymorphic constant i.e. a value that can take on any type (no
      restrictions)

# Two namespaces (types, values)

* Haskell code is split up into two namespaces

1. Types
2. Values

The type namespace is

* after :: (the stuff on the left is in the value namespace) in function
  declarations and in record syntax
* after `type`
* between `class` and `where`
* between `instance` and `where`
* between `data` and `=`
# Operator Precedence

* There are 10 levels of operator precedence (0-9)
* function application is always higher (kind of at level 10)
* the infix notation will make any function an operator at level 4
    * you can change this for individual functions. How ???
* Operators always take exactly 2 arguments
* An operator (even one created by \`\`) can be written using prefix notation using () e.g.
    * (+) 3 5
    * (==) 3 3
    * (`foo`) 4 'a'
    * these are called `sections` and allow you to curry operators just like
      functions

## The apply operator `$`

* purpose: allows us to write less parantheses
* does function application but
    1. has operator precedence 0 (the lowest)
    2. normal function application is left associative but `$` is right associative e.g.
        * normal:   f g h x -- <-- this reads nicely (from right to left)
                    (f g) h) x
            * but to make it work  f, g, h need to return functions which makes
              them less directo

        * `$`:      f $ g $ h x -- <-- this reads nicely too
                    f (g (h x))
            * Here f,g,h can all be simple functions that just work on `x`

## The compose operator `.`

Definition of it:

```haskell
(f . g) x = f (g x)
```

## Indentation rules

Code which is part of some expression should be indented further in than the beginning of that expression

All grouped expressions must be *exactly* aligned

There are 4 layout keywords: `let`, `where`, `do`, `of`
* They begin _layout blocks_
* Under the hood Haskell encloses these in `{` and `;`


* If the compiler finds a `{` after one of the layout blocks it will parse the
  block directly (not using the layout rules)

The layout process can be summed up in three translation rules (plus a fourth one
that doesn't come up very often):

1. If you see one of the layout keywords, (let, where, of, do), insert an open curly brace (right before the stuff that follows it)
2. If you see something indented to the SAME level, insert a semicolon
3. If you see something indented LESS, insert a closing curly brace
4. If you see something unexpected in a list, like where, insert a closing brace before instead of a semicolon.

```
-- this does not work:
do  thing
    if condition
    then foo
    else bar
    anotherThing

-- because it becomes:

do  { thing
    ; if condition
    ; then foo
    ; else bar
    ; anotherThing }

-- so the solution is
do  thing
    if condition
        then foo
        else bar
    anotherThing

-- because it becomes:

do  { thing
    ; if condition
        then foo
        else bar
    ; anotherThing }

-- because `if` is not a layout keyword.

-- This only happes in do blocks but to avoid issues you should always indent
-- the then and else part of an if expression a bit more than the `if` part.
```

Things which are considered "one line" in haskell (in the sense of no `{` or `;`
being inserted):

* function definitions
    ```
    doubler
        x
        =
        x
        +
        12
    ```
* if then else expressions (semicolons are not allowed within an if-then-else)

# Lambdas

```
\x -> (x, 3, 4)
```

* a lambda extends from the `\` to the next newline|;|)|}|]
* you can do pattern matching in lambdas but it is rarely used since you can
  only write one branch and if it fails you will get a runtime exception

Aside: when pattern matching make sure you have covered all the possibilities or
you will end up with a runtime exception!



# Do syntax

* is syntactic sugar for `>>=` and `>>` operators and lambda notation
