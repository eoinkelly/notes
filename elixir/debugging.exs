# Debugging
# #########

# Sources
# * http://blog.plataformatec.com.br/2016/04/debugging-techniques-in-elixir-lang/


# NOTE:
# debugging doesn't work out of the box before erlang 19 - see
# http://blog.plataformatec.com.br/2016/04/debugging-techniques-in-elixir-lang/
# for how to make it work for erlang 18.


# First we start debugger ...
#
#   :debugger.start()
#
# ... then ???  ...
#
#   :int.ni(InterestingThing)
#
# ... then set breakpoint on line 10 of InterestingThing ...
#
#   :int.break(InterestingThing, 12)

defmodule InterestingThing do
  def some_func do
    IO.puts "hello"
    IO.puts "there"
    blah = 44
    bar = "hi"
    foo = 33
    foo + blah
  end
end

# pry
# ###

# To use IEx.pry:
#
# 1. Add `require IEx to module you are interested in
# 2. Add `IEx.pry` to the line you want to stop on


# Example:
# require IEx
#
# defmodule PryPlay do
#   def some_func do
#     one = 1
#     IO.puts "before pry"
#     IEx.pry
#     IO.puts "after pry"
#     "ret val"
#   end
# end
#
# PryPlay.some_func

# run this as:
#
#   $ iex debugging.exs
#
# and you are dropped into the pry session. Exit the session by typing `respawn`

# About pry
# #########
#
# * pry cannot do "debugger" things like step, breakpoints etc.
# * pry is for "non production" debugging - why ???
# * it runs int he caller process and blocks the caller
# * from pry we can see
#     * bindings
#     * lexical information - how ???
#     * process information - how ???



# Using pry in tests
# ##################

# TODO: this does not work yet

# Given the example test ...

# ExUnit.start
#
# defmodule PryTests do
#   use ExUnit.Case #, async: true
#
#   test "the truth" do
#     one = 1
#     IEx.pry
#     assert one + one == 2
#   end
# end


# when we run it as
# iex -S mix test --trace
#
# * --trace is a flag to mix (not iex) that has the side effect of stopping the test session from timing out
