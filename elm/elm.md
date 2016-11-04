# Elm

Core concepts

1. values
2. functions
3. lists
4. tuples
5. records

## values

```elm
-- line comment

"foo"
-- "foo" : String

-- values are immutable e.g. if you edit record then it creates a new copy
"foo" ++ "bar"
-- "foobar" : String

4
-- 4 : number

4.5
-- 4.5 : Float

-- * has Integers (`Int`) and floating point (`Float`) numbers
-- * `//` for Integer division
-- * `/` for floating point division

> 9 / 3
3 : Float

> 9 // 3
3 : Int

-- always bring the body of a function down a line (exception is the repl)
isNeg n =
    n < 0
> isNeg n = n < 0
<function> : number -> Bool

> isNeg 5
False : Bool

> isNeg -3
True : Bool

-- if then else
-- * is a construct that always returns a value
-- * has not curly braces
-- * it seems each branch can have a single expression only
> if isNeg -3 then "less than 0" else "greater than 0"
"less than 0" : String

-- has no truthiness - True is the only truthy value

-- Lists
-- * are homogenous (one type only)
-- * list functions live in List

> names = ["eoin", "nahum", "james"]
["eoin","nahum","james"] : List String

> names
["eoin","nahum","james"] : List String

> List.isEmpty names
False : Bool

> List.length names
3 : Int

> List.sort names
["eoin","james","nahum"] : List String

> names
["eoin","nahum","james"] : List String

> List.reverse names
["james","nahum","eoin"] : List String

> List.head names
Just "eoin" : Maybe.Maybe String

> List.head []
Nothing : Maybe.Maybe a

-- Tuples
-- * wrapped in parens

> (True, "good value")
(True,"good value") : ( Bool, String )

-- Records
-- * set of key-value pairs
-- * surrounded by curly braces, separated by comma
-- * a bit like structs in other languages
-- * creating a record automatically creates getter functions named `.NAME_OF_ATTR`
-- * strict
--      * cannot have missing/null/undefined values
--      * will explode if you ask for a record that does not exist
-- * elm has no `this` or `self` keywords
-- * Records support "structual typing" - if the record has the correct fields for a funciton then it can be used by that function.
-- * you can use pattern matching in function defintions to split apart records for use by the functionj

> point = { x = 3, y = 4 }
{ x = 3, y = 4 } : { x : number, y : number' }

> .x point
3 : number

-- no pattern matching
> isInGrid point = point.x < 100 && point.y < 100
<function> : { a | x : number, y : number' } -> Bool

-- using pattern matching to split the record
> isInGrid2 {x, y} = x < 100 && y < 100
<function> : { a | x : number, y : number' } -> Bool
```

Number types

* uses spaces for function application
    * args separated by whitespace not comma
* boolean values: `True` and `False`


# elm-repl

* `elm-repl`
* cannot parse type annotations


Types

it has type variables

```
> []
[] : List a
```

Type aliases

* very handy for naming a particular kind of record


Union types (aka tagged unions, algebraic data types)

type Msg = Reset | Increment | Change String

* there are three constructors of a Msg

* Reset is a function that takes no args and returns a Msg
* Reset is a constructor for User values
* Reset is also a type of value
* I think elm creates both for you at the same time ???
