# # Function calls
#
# Brackets are optional
#
#     String.capitalize("arg")
#     String.capitalize "arg" # same as above
#
# A single function can have many bodies - all take same number of args (same arity)
# * elixir chooses a body based on pattern match of the args
#
### Bodyless function heads
#
# Uses for bodyless function heads:
#
# 1. Documentation
#     * the elixir compiler can show docs somehow???
#     * when the elixir compiler is autogenerating docs for a function it infers
#       the argument names from the code. If your first function head is a pattern
#       match this can lead to unclear argument names in the docs
# 1. Default arguments
#     * if a function with default values has multiple clauses, it is required to
#       create a function head (without an actual body) for declaring defaults
# 1. Protocols
#     * functions in a protcol are declared using a bodyless function head

IO.puts "HI"

defmodule Stuff do
  def sum(a, b // 0, c // 0)

  def sum(a, b, c) do
    a + b + c
  end
  def sum(a, b) do
    a + b
  end
  def sum(a) do
    a
  end
end

defprotocol Blah do
  def do_thing(x)
  def valid?(data)
end
