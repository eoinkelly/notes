# Modules
# #######

# the following achieve the same thing

defmodule Foo1 do
  defmodule Bar1 do
    def some_func do
      IO.puts "ret val"
    end
  end
end

defmodule Foo2.Bar2 do
  def some_func do
    IO.puts "ret val"
  end
end

Foo1.Bar1.some_func
Foo2.Bar2.some_func

# => Elixir "nested" modules are syntax sugar - they are just a naming convention

# modules cannot be reopened

# Each module definition wipes previous definitions. It is not like ruby where
# you can reopen modules and classes to adjust them.

defmodule Once do
  def thing, do: IO.puts "thing #{other}"
  def other, do: "other"
end

Once.thing

# this will result in a warning about redefining a module but more importantly
# will cause anything previously defined in Once to be discarded.
defmodule Once do
  def thing do
    IO.puts "redefinition"
    # IO.puts "second thing #{other}" # fails because other is not a function now
  end
end

Once.thing
