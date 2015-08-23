# Docstrings
# ##########

# * @doc is an "attribute" of the module so has the same rule as other
#   attributes
# * The @doc attribute is set to a string (usually a multiline heredoc string
#   for convenience)

defmodule Times do
  @doc """
  I am a multiline string (heredoc)
  """
  def double(n) do
    n * 2
  end

  # @doc is an attribute so cannot be within a method
  @doc ~S"""
  I am a multiline string (heredoc) created with the ~S sigil
  which has advantages ????
  """
  def adder(a,b,c) do
    # here we are putting a multi-line string as the first expression in the
    # method. Its evaluation is ignored so this is technically valid elixir
    """
    I am a multiline string (heredoc)
    """
    a + b + c
  end
end

