# Introspection
# #############

# iex> h(some_thing) # show whatever help is defined for that term
# iex> i(some_thing) # dumps details of the type, protocols, module etc.
#
# # Finding which module a function is defined in:
# iex> IO.inspect(&methodname/arity)
# iex> IO.inspect(&redirect/2) # example
#
# # Showing all the functions in a module
# iex> SomeModule.__info__(:functions)

# Debugging
# #########

xx = {3, 5, "hello"}

# Sources
# * http://blog.plataformatec.com.br/2016/04/debugging-techniques-in-elixir-lang/
#
# There are 3 main ways to debug Elixir code:
#
# 1. Print debugging
# 2. IEx.pry/0
# 3.Graphical Debugger
#

# 1. Print debugging
# ##################
#
# There are 3 releveant functions here:
#
# 1. IO.puts
# 2. IO.warn
# 3. IO.inspect
#

IO.puts("got to here i.e. this is an execution point flag")

# IO.puts/1 can be used with Kernel.inspect/1 to see the contents of variables
IO.puts("Value of xx: #{inspect(xx)}")

yy = IO.inspect(xx, label: "Value of xx")

if yy === xx do
  IO.puts("xx and yy were equal as expected")
else
  raise("yy should have been equal to xx")
end

##
# IO.inspect returns the given value unchanged so you can insert it into
# assigments and use it in pipelines!
#
defmodule IOInspect.Example do
  def do_something(thing) do
    # This is the original line
    do_something_with(thing)
    # ... and this is the debuggable version
    do_something_with(IO.inspect(thing, label: "this is thing"))

    # IO.inspect can also be inserted into pipelines
    thing
    |> IO.inspect(label: "before")
    |> wrap("this was wrapped by Z")
    |> IO.inspect(label: "after")
  end

  defp wrap(thing, msg) do
    {:wrapped, thing, msg}
  end

  defp do_something_with(_x), do: nil
end

IOInspect.Example.do_something(xx)

# IO.inspect has many options, some of which you will want to adjust
# IO.inspect(xx,
# label: "some label",
# limit: :infinity,
# printable_limit: :infinity,
# structs: true|false
# as_binaries: true
# as_lists: true|false
# width: 80
# pretty: true|false
# )

# IO.warn is a handy way to generate a stacktrace at a particular point in your
# code
IO.warn("This is an IO.warn message - it includes a stack trace by default")

# 2. IEx.pry/0
# ######

# To use IEx.pry:
#
# 1. Add `require IEx to module you are interested in
# 2. Add `IEx.pry` to the line you want to stop on
#
# Example: Require and invoke it on the same line (the formatter will rewrite
# this as two lines but thats not a big deal)
# require IEx; IEx.pry()

# Common IEx Commands
#
# * respawn
# * continue
# * whereami
# * open # opens the current file in $EDITOR
#
# ExUnit has a 60sec timeout for paused tests - this breaks pry sessions. Run ExUnit iwth --trace to disable this

# 2.5. IEx.break/0
# TODO

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
# * it runs in the caller process and blocks the caller
# * from pry we can see
#     * bindings
#     * lexical information - how ???
#     * process information - how ???
#

# Using pry in tests
# #################

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
# * --trace is a flag to mix (not iex) that has the side effect of stopping the
# test session from timing out

# 3.Graphical Debugger
# ####################

# NOTE:
#
# Debugging doesn't work out of the box before erlang 19 - see
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
# ... then set breakpoint on line 12 of InterestingThing ...
#
#   :int.break(InterestingThing, 12)

defmodule InterestingThing do
  def some_func do
    IO.puts("hello")
    IO.puts("there")
    blah = 44
    bar = "hi"
    foo = 33
    foo + blah
  end
end
