# Pattern matching

> Elixir does not have mutable variables, it has rebinding

Good overview of why:
http://blog.plataformatec.com.br/2016/01/comparing-elixir-and-erlang-variables/

## Pattern matching using `=`

- `=` is NOT assigment, it is the "pattern match operator"
    - a binary operator, takes a pattern and a THING (any kind of elxir type)
    - returns THING if the match succeeded
    - raises a `MatchError` exception if it fails
        - => errors are exceptions, you must be ready for the program flow to
          stop if a match fails
        - the exception message will include THING
- PATTERN
    - must be an elixir type (either a value type or collection type)
    - it is a pattern for finding parts of an elixir type (either value or
      collection type)
    - use `_` to acknowledge parts of the structure of THING without having to
      bind variables to them
    - variables bind **once per match** i.e. if your pattern includes variable
      `a` then the first thing it binds to in the match will be its value at the
      end of the match. If the pattern tries to re-bind `a` then the match will
      fail and raise a `MatchError`
        - Erlang does not all re-binding of variables anywhere. Elixir only
          prevents it within a single match attempt
- THING
    - if THING starts as an expression the elixir will execute it until it boils
      down to a single (arbitrarily complex) data type
    - any elixir data type
        - can be arbitrarily complex and nested

```
PATTERN = THING
# or in pseudocode
attempt_to_match(PATTERN, THING)
```

## Pattern matching in function heads

- same as with `=` except that the function definition includes the PATTERN and
  the arguments the fuction gets are THING

```
def do_stuff(PATTERN) do
  # ...
end

do_stuff(THING)
```

## The ^ (or 'pin') operator

- allows you to use a variable's value in a pattern and explicitly say that you
  do not want elixir to rebind a new value to that variable in the match
- allows you to say "use the value in this variable as a literal in this
  pattern, not as a variable to bind to
- this is important sometimes because pattern matching will default to binding
  variables and sometimes we don't want that
