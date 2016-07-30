defmodule Math do
  defmacro say({:+, _, [lhs, rhs]}) do
    quote do
      real_lhs = unquote(lhs)
      real_rhs = unquote(rhs)
      result = real_lhs + real_rhs
      IO.puts "#{real_lhs} plus #{real_rhs} equals #{result}"
      result
    end
  end

  defmacro say({:-, _, [lhs, rhs]}) do
    quote do
      real_lhs = unquote(lhs)
      real_rhs = unquote(rhs)
      result = real_lhs - real_rhs
      IO.puts "#{real_lhs} minus #{real_rhs} equals #{result}"
      result
    end
  end
end

# iex(3)> c "metaprogramming/math.exs"
# [Math]
# iex(4)> require Math
# Math
# iex(5)> Math.say  3 + 4
# 3 plus 4 equals 7
# 7

# Elixir knows magically when the thing is a macro that
# it shouldn't evaluate the args but should instead
# pass the AAST of them to the macros


