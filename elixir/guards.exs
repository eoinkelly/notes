defmodule Guard do
  # * guard clauses are intended to choose a function head based on some
  #   inspection of the types and values of the args
  # * not all expressions can be used in guard clauses
  def type?(x) when is_number(x), do: "Number"
  def type?(x) when is_list(x), do: "List"
  def type?(x) when is_atom(x), do: "Atom"

  # catch-all function head with
  def type?(_x), do: "Unknown"
end

defmodule OverlyFunky do
  # default params
  def odd_params(a, b \\ "default b", c \\ "default c", d) do
    IO.inspect %{ a: a, b: b, c: c, d: d }
  end

  # function with multiple heads
  # notice how the default param is set in a head with no body
  def blah(a, b \\ 12)

  def blah(a, 33) do
    IO.inspect %{a: a, b: "thirty three"}
  end

  # put most general function head at the end!
  def blah(a, b) do
    IO.inspect %{ a: a, b: b }
  end

end

# OverlyFunky.odd_params(1) # error
OverlyFunky.odd_params(1, 2)        # => %{a: 1, b: "default b", c: "default c", d: 2}
OverlyFunky.odd_params(1, 2, 3)     # => %{a: 1, b: 2, c: "default c", d: 3}
OverlyFunky.odd_params(1, 2, 3, 4)  # => %{a: 1, b: 2, c: 3, d: 4}
