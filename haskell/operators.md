# Operators

```haskell
-- infix operator examples
x ^ a   -- where a is an integer
x ** a  -- where a is any number type
x % y   -- get ratio between two number. x, y must be of same type
```

- Haskell lets you make new operators (which are just binary functions)
- operators can be applied like normal functions by wrapping them in `()`

```haskell
(+) 3 4 -- 7
```

```haskell
ghci> :info *
infixl 7
-- read as precedence level 7, associates to the left
```

# Operator Precedence

- There are 10 levels of operator precedence (0-9)
- function application is always higher (kind of at level 10)
- the infix notation will make any function an operator at level 4
    - you can change this for individual functions. How ???
- Operators always take exactly 2 arguments
- An operator (even one created by \`\`) can be written using prefix notation
  using () e.g.
    - (+) 3 5
    - (==) 3 3
    - (`foo`) 4 'a'
    - these are called `sections` and allow you to curry operators just like
      functions

Haskell Operator precedence:

```
| *     | infixl | 7 |
| `div` | infixl | 7 |
| `mod` | infixl | 7 |
| +     | infixl | 6 |
| -     | infixl | 6 |
| $     | infixr | 0 |
```

- Note that the precendence of `+` and `-` is the same
- Note that the precedence of `div` and `*` is the same
- You can use parentheses to specify order of operations.
    - QUESTION: How does this not get confused with creating a tuple?
- When an expression contains multiple operators with the same precedence,
- Evaulation happens according to the associativity e.g. `6 - 3 + 4` is
  `(6 - 3) + 4`

Compare Haskell to the precedence rules in C?

```
| *     | infixl | 13 |
| /     | infixl | 13 |
| %     | infixl | 13 |
| +     | infixl | 12 |
| -     | infixl | 12 |
```

We can see that the relative precedence and associativity is pretty similar. It
seems like most languages use similar rules for associativity, precedence and
semantics of mathematical operators.

## Fixity

- `fixity` is the concept of precedence and associtiavity together

## The apply operator `$`

- purpose: allows us to write less parantheses
- does function application but
    1. has operator precedence 0 (the lowest)
    2. normal function application is left associative but `$` is right
       associative e.g.
        - normal: f g h x -- <-- this reads nicely (from right to left) (f g) h)
          x
            - but to make it work f, g, h need to return functions which makes
              them less directo

        - `$`: f $ g $ h x -- <-- this reads nicely too f (g (h x))
            - Here f,g,h can all be simple functions that just work on `x`
