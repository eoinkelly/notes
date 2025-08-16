## alias

- creates aliases for modules **only**
- is lexically scoped
    - can be used with modules or functions
- does not change what functions and macros are available to your module
- use-cases:
    - saves you typing by not having to type the full name of a module every
      time (common)
    - lets you rename a module to make its use clearer in your context (rare,
      maybe not a good idea anyway)

Examples

```elixir
# canonical name of a module in elixir
Elixir.MyApp.Finance.Calculator

# shorter name that Elixir allows you to use in all lexical scopes
MyApp.Finance.Calculator

# lets your refer to 'Calculator' in the same lexical scope you defined this alias
alias MyApp.Finance.Calculator

# lets you refer to 'Calcy' in the same lexical scope you defined this alias
alias MyApp.Finance.Calculator, as: Calcy
```

## require

- macros in Elixir are opt-in - you cannot use a macro in a module without
  having something in the code to explicitly pull in that macro
- is lexically scoped so can be used within modules or functions
- makes **all** the macros in the named module available to the current module
- Note: the public functions of every module are available at all times -
  `require` is only required for macros
- the macros are still in the namespace of the module that defined them i.e.
  they must still be invoked by prefixing them with the name of the module e.g.
  `require Bar; Bar.my_macro`
    - look at `import` if you need to pull functions or macros into the
      namespace of the current module
- is automatically run when you `import` a module

```elixir
defmodule Foo do
  # All macros defined in Bar will be available for Foo to call now
  require Bar
end

defmodule Foo2 do
  def do_thing do
    # All macros defined in Bar will be available for just this function
    require Bar
  end
end
```

## import

- sort of a "require module `Foo` and pull some/all functions & macros from
  `Foo` into my namespace" command
- pulls functions or macros from other modules into the current modules
  namespace
- is lexically scoped
- can be quite surgical via the `only: []` and `except: []` options
- Side-effects:
    - importing a module automatically requires the module so **all** macros
      from the module are available under the namespace of the required module.
      This happens even if you use `only: []` or `except: []` options

```elixir
defmodule A do
  def say_hi do
    IO.puts("hi")
  end

  def say_bye do
    IO.puts("bye")
  end

  defmacro macro_if(clause, do: expression) do
    quote do
      if(unquote(clause), do: unquote(expression))
    end
  end
end

defmodule B do
  # Note that even though we only imported the say_hi function we got an
  # implicit `require A` which made the macro available to us under the `A`
  # namespace
  # import A, only: [say_hi: 0]

  # Note that even though we only imported functions we got an
  # implicit `require A` which made the macro available to us under the `A`
  # namespace
  import A, only: :functions

  def do_thing do
    say_hi()

    A.macro_if true do
      IO.puts("Should be printed")
    end
  end
end

B.do_thing()
```
