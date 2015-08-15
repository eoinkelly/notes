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
