# Whitespace and Indentation

Sources

* http://en.wikibooks.org/wiki/Haskell/Indentation

*Any* kind of whitespace is also a proper delimiter for lexemes. This means
that newline is the same as space and tab as far as the compiler is concerned.

## Indentation rules

1. Code which is part of some _expression_ should be indented further in than
   the
   beginning of that _expression_  (even if the expression is not the leftmost
   element of the line)
2. All grouped expressions must be *exactly* aligned

Things which are expressions in Haskell

* function definitions
* let blocks
* do blocks
* where blocks
* of blocks
* if-else ???
* ???

There are 4 layout keywords: `let`, `where`, `do`, `of`

* They begin _layout blocks_
* Under the hood Haskell encloses these in `{` and `;`
* If the compiler finds a `{` after one of the layout blocks it will parse the
  block directly (not using the layout rules)

The layout process can be summed up in three translation rules (plus a fourth
one that doesn't come up very often):

1. If you see one of the layout keywords, (let, where, of, do), insert an open
   curly brace (right before the stuff that follows it)
2. If you see something indented to the SAME level, insert a semicolon
3. If you see something indented LESS, insert a closing curly brace
4. If you see something unexpected in a list, like where, insert a closing
   brace before instead of a semicolon.

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

-- and if-then-else is a single expression that we have split with `;` so the
-- solution is

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

# Functions and if-then-else are "One line" in Haskell

Things which are considered "one line" in haskell (in the sense of no `{` or
`;` being inserted):

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

## Aside: `_`

* `_` is a reserved identifier in Haskell
* compilers are encouraged to supress warnings about unused variables that
* _begin with_ it.
* It is a way of telling the complier "I expect this to be unused"

# Indentation and module

Q: so why can't i make a funciton with all tokens starting in first line
    does every file have an implicit module wrapper? if so I am always
    within a where block
    => all new statements must start at the same level of indent
    => if i put my function bits all at same indent the compiler thinks they
        are separate expressions

QUESTION: is there an implict where at top of all haskell files?

