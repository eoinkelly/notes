# modules cannot be reopened - the final opening overwrites all others
defmodule Times do
  def double(n) do
    n * 2
  end

  # doc strings must be outside the definition
  # @doc """
  # The 2 arg version of adder
  # """
  # def adder(a,b) do
  #   a + b
  # end
  def adder(a,b), do: a + b

  @doc """
  The 3 arg version of adder
  """
  def adder(a,b,c) do
    a + b + c
  end
end
