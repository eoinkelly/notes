# Metaprogramming Elixir

* THe AST is very close to hand in elixir
    * it is represented by standard elixir data structures (tuples, lists, keyword lists)
    * there are built-in macros for working with it
* a lot of elixir syntax is implemented as macros e.g. `if`, `defmodule`, `def`, `Logger.debug`
    * check out `h if` in iex
* macros are replaced during compilation (they never make it to runtime)
    * given a piece of elixir code the compiler will
        * find the macros in it and replace them with the AST they return
        * repeat the previous step on the resulting code until all macros have been evaluated
* neat trick:
    * the `Logger` module can completely remove logging statemsnts from the AST for a particular environment
* Elixir knows somehow when the function is a macro that it should pass the AST of the args, not evaluate the args and pass the result

## Macros

* Most important: macros receive ASTs as arguments and return ASTs

* `quote do: <ELIXIR_EXPRESSION>`
    * returns the AST for the given expression
    * does not evaluate the expression
* `unquote`
    * injects values from an outside context into the AST
        * similar to how string interpolation lets you inject other values into a string context

The inside of a quote block cannot see any bindings defined outside it so if you refer to a variable you defined outside the quote block will think you are calling a function with that name

Unquote says "look outside the quote block for a binding of this name and put its value in here"

```elixir
iex(1)> number = 5
5
iex(2)> ast = quote do
...(2)> number * 4
...(2)> end
{:*, [context: Elixir, import: Kernel], [{:number, [], Elixir}, 4]}
iex(3)> Code.eval_quoted ast
** (CompileError) nofile:1: undefined function number/0
    (stdlib) lists.erl:1353: :lists.mapfoldl/3
    (elixir) lib/code.ex:200: Code.eval_quoted/3
iex(3)> ast2 = quote do
...(3)> unquote(number) * 4
...(3)> end
{:*, [context: Elixir, import: Kernel], [5, 4]}
iex(4)> Code.eval_quoted ast2
{20, []}
```

```elixir
# TODO these are not the same?
iex(9)> quote do: 4 + 5
{:+, [context: Elixir, import: Kernel], [4, 5]}
iex(10)> quote do: Kernel.+(4,5)
{{:., [], [{:__aliases__, [alias: false], [:Kernel]}, :+]}, [], [4, 5]}
```

## AST structure

* every complex elixir expression is represented as a three element tuple `{FUNC_ATOM_OR_NESTED_TUPLE, METADATA, ARGS_LIST}` in the AST
    1. an atom denoting the function call OR another three-tuple representing a nested node in the AST
    2. metadata about the expression
        * held in elixir keyword list
            * revision: keyword list is shorthand for a list of two-tuples where
                * first tuple element is always atom.
                * keys are ordered
                * keys can appear multiple times
        * keys
            * `:context`
                * seems to always have value `Elixir` ???
            * `:import`
                * I think this is the module that the function comes from
    3. list of arguments for the function call
        * held in elixir list
* an exception to the above rule is that several literals in Elixir have the
  **same** representation in the AST as they do in elixir code
    * examples
        * atoms
        * integers
        * floats
        * lists
        * strings
        * any two-element tuple containing the above types
    * the important consequencs of this is that if we pass any of the above
      types to a macro, the macro will receive them as-is instead of a
      three-tuple representing them

```elixir
iex(11)> quote do: :foo
:foo
iex(12)> quote do: {:foo, [3,4]}
{:foo, [3, 4]}
iex(13)> quote do: "hello"
"hello"
```

Kernel.SpecialForms

* the `case` macro is one of these built-in "special forms"
* it is a macro but cannot be overidden
* I _think_ that a lot of elixir's syntax (stuff other than function calls) is implemented as macros

Useful macro functions

* `Code.eval_quoted(<SOME_AST>)`
    * evaluate a chunk of AST
* `Macro.expand_once(<SOME_AST>)`
    * do one layer of macro expansion on the given AST

* A "context" is the scope of bindings, imports and aliases
    * does "imports" include macros???

* Macros **Inject** code into the caller's context (they don't just generate and return it
* macros have the **potential** to change things in the caller's context
    * this is potentially very confusing so has to be done carefully

