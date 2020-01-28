# Lua

* is a _tabled based_ programming language
* has 21 keywords
* 5.3 supports explicit integers and bitwise operators
* has both
    1. an interpreter + repl `lua`
    2. compiler `luac`
* uses prototype objects - like JS
    * Classes and objects are not separate concepts
    * You con't create a blueprint for an object (class) and then create an
      intance of it (object). Instead you just create an object and clone it to
      make new instances.
* is dynamically typed
* variables can be assigned values of a new type
* Standard library
    * lua has a standard lib and it seems to be just magically available in all lua code
* Lua will do type coercion pretty freely:
    ```lua
    > "10" + 1
    11.0
    > "10" .. 123
    10123
    ```
* Scope
    * lua calls scopes chunks
    * There is a global scope
        * Every file has its own scope
            * every `do end` opens a new scope
            * every function definition opens a new chunk/scope
    * Scopes have access to everything in their (lexical) parent scope
* variables declared without `local` are global **no matter what chunk they are declared in**
* logical operators vs relational operators
    * `==` and `~=` are relational operators
        * they compare two values and **always** return a `boolean` result
    * `and`, `or` are logical operators - they always return **one of their operands**
        * `and` returns the second operator if both are true
        * they do NOT evaluate to a boolean result
        * `and` and `or` both use shortcut evaluation - avoid combining them with functions with side-effects
    * `not` is a logical operator but it does return a `boolean`
    ```lua
    k

    ```
* only `false` and `nil` are falsy, everything else is truthy
* iteration
    * use `pairs()` to yield **all** keys (and their values) from a table
    * use `ipairs()` to yield only numeric keys (and their values) from a table
    * In the syntax below, ITERATOR_FUNC can be any function that returns enough values as it is called repeatedly - the first time it returns `nil` the iteration will stop. Combined with returning an anonymous function which accesses variables declared outside it in your ITERATOR_FUNC, your ITERATOR_FUNC can statefully walk across a data structure pretty easily
        ```lua
        for VARIABLES in ITERATOR_FUNC do
        end
        ```
* global variables in lua live in an invisible table exposed as `_G`
    ```lua
    -- all these are the same thing
    foo = 12
    _G.foo = 12
    _G["foo"] = 12
    ```
* You can get at the table which holds all the `local` variable assignments within a function via `_ENV` within that function
    * `_ENV` points at `_G` in the global scope
* Lua doesn't have built-in classes but you can build a prototypal inheritance system pretty easily with metatables.
    * create a "prototype" object
    * create a constructor function which returns a new object which has a metatable where the `__index`   has been set to point at the prototype so that any missing values will be looked up in the prototype

* if you call a function in a table with `:` instead of `.` it will automatically pass in a reference to the table as a first object to the function
    * this bit of sugar makes the lua code look more OOish

## Modules

* Lua has multiple ways of creating and loading modules - this is just one method

* a module is a lua table
* a module file is a lua file which returns a table
* basically you build up a table in your file and then `return` it at the top level of the file
* `require(filenameWithoutLuaExtension)`
    * evaluates the  file?
    * is smart enough not to reload a file it has already loaded (similar to Ruby `require`)
    * returns a table
    * it searches for the file on `package.paths` (a `;` separated string of paths)
* you can save state into a file as lua code and then later `require` that file and lua will build a table for it
    * this means you don't have to serialize/deserialize the state into another format like JSON/YAML etc.
        * obviously there are downsides to this too but it seems useful in the right time & place
* if you need an equivalent of Ruby `load` then it is `dofile(pathToFile)` which will just reload the file every time you call it
    * it does not  search `package.path`

## Types

Lua has 8 basic types

1. nil
    * is the value that variables which have not been assigned have
    * any unrecognised identifier in your code will return `nil` e.g.
    ```lua
    > print(asdfasdl)
    nil
    > asddsfff
    nil
    ```
1. boolean
    * values are: true|false
    ```lua
    foo = true
    bar = false

    a == b -- test for equality
    a ~= b -- test for inequality
    ```
1. number
    * seems to be floating point like JS
1. string
    * literals can be enclosed in single or double quotes
    * double quotes seem to be more conventional
    * examples
        ```lua
        -- escape chars work with any kind of quote
        > "he\nlo"
        he
        lo
        > 'he\nlo'
        he
        lo
        ```
1. function
    * names seem to be `CamelCasedWithInitialCap()`
    * like old-school JS you can pass more args to a function than you have declared params
        * if you pass fewer args the missing values are filled in with nil
    * return `nil` if you don't add an explicit return statement
        * quirk: `return` must be followed by `end` so you have to wrap early returns in `do end` e.g.
    * functions are first class, you can pass them around as values
        ```lua
        function buildThing()
          -- other stuff

          -- return anonymous function
          return function()
            print("hi")
          end
        end
        ```
    * functions act as closures over their surrounding lexical scopes just like in JS
    * can return multiple values by just separating them with commas
    * if you return N values but assign fewer then the extra values are just discarded
    * keys are accessed by `[]` but if keys are strings you can use `.`
    * if you use a key which doesn't exist you get `nil` back
    * table "constructor" is the literal syntax
        ```lua
        x = {
            k1 = "hi"
            k2 = 1234
        }
        ```
    * tables are assigned and passed to functions by reference
    * Arrays
        * are tables with numeric keys
        * lua does NOT support multidimensional arrays directly but you can do them as nested tables
        * indices start at 1 not 0
            * you can use any keys you want but
                * `#` length operator will only count keys starting at 1
                * `#` considers an array to be finsihed if it finds two `nil`s in a row so be careful with sparse arrays!
        * lua might actually allocate the "array" contigiously in memory iff
            1. all table keys are nubmers
            2. numeric indeces start at 1 (not 0!!!)
            3. at least half the indices are not nil
        * have a construdtor similar to C
            ```lua
            myarry = { "hello", "here", "hi" }
            ```
1. table
    * a set of key-value pairs
    * array-meets-dictionalry object that can be used for many things
    * the only data structure in lua
    * any type except `nil` can be a key, values can have any type
1. userdata
1. thread
    * used to run code in parallel - how?

Note: no array type

Are semicolos required?

```lua
-- are comments

-- assignment works as you would expect
foo = "hello"

-- multiple assignment works too
a, b = 123, "hi"

-- print() is a bit like console.log - it will print anything you give it
```

## Control structures

```lua

if SOME_BOOLEAN_CONDITION then
  -- statements
elseif SOME_OTHER_CONDITION then
else
end

while CONDITION do
  break -- works as you would expect
end

repeat
  -- statements
until CONDITION

-- for loops are very constrained compared to other langs
-- they can only assign one variable which must be numeric and they force you to increment/decrement it
-- they are really just for counting through a set of numbers
for
```

## Meta tables

* meta tables are just tables with some keys which have pre-arranged meanings
* you can attach a metatable to a table with `setmetatable(targettable, metatable)`. After that, lua will look for certain keys in the metatable when you try to perform certain actions on the table.
* tables have a `nil` metatable by default
* `getmetatable(table)` will return the meta table for `table`

Useful metatable methods

* `__index = function(containertable, key)` value is a function which will be called if an unknown key is requested from the container table
    * acts like a getter  but it called for all keys in the table
    * `rawget(table, key)` can be used to bypass `__index`
* `__newindex = function(containertable, key, value)` called when a new value is added to the table
    * acts like a setter but is one setter called for all keys in the table
    * `rawset(table, key)` can be used to bypass `__newindex`
* `__call = function(containertable, otherargs...)` lets you call a table like a function, passing  in whatever args you like
    * lua calls this a _functable_ but other langs call it _functor_
    * functables let you couple state with a function


## Built-in functions

```lua

type() -- -> string describing the type

math.floor()
math.ceil()


string.len()

-- # is the length operator (a unary operator)
#"string literal" -- same as string.len("string literal")

.. -- concatenate strings
tostring() -- explicitly coerce value to string


-- read from stdin
my_input = io.read()
```
