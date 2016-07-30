# directives
# ##########

# import
#
# * makes all the functions and macros in the named module be in the current
#   modules namespace
# * can be filtered with
#     * `only: [func_name: arity, func_name2: arity2]`
#     * `except: [func_name: arity, func_name2: arity2]`
# * defp private methods cannot be imported

# alias
#
# * make a short name for an existing, in scope module (does not
#   work on functions)
# * works within a module or within a function

# require
# * pulls in macros only!
# * memonic: ??

# use
# * does a require then calls the __using__/1 function in the required module
# * a convenience for pulling in macros
# * Note: it does NOT pull in functions!

defmodule Hewey do
  def quack do
    IO.puts "Hewey quack"
  end
end

defmodule Dewey do
  alias Hewey, as: Hew # alias works at module level

  def squawk do
    alias Hewey, as: Hew # alias works within a function
    IO.puts Hewey.quack # works
    IO.puts Hew.quack # works
  end
end
# Dewey.squawk

# import
# ######

defmodule Pinky do
  def pub, do: "public"
  def pub2, do: "public 2"
  defp priv, do: "private"
end

defmodule Brain do
  # can add only:, except: to whitelist,blacklist functions to import
  import Pinky, except: [pub2: 0]

  def show do
    IO.puts pub
    # IO.puts pub2 # does not work because it was not imported
    # IO.puts priv # private methods cannot be imported
  end
end
# Brain.show

# require
# #######

# require only pulls in macros

# use
# ###

# define a module
defmodule Jerry do
  def __using__([version: version]) do
    IO.puts "hi from jerry __using__/1 got #{version} arg"
  end
end

defmodule Tom  do
  use Jerry, version: "0.3.4"
  # is equivalent to
  # require Jerry
  # Jerry.__using__([version: "0.3.4"])

  def some_func do
    IO.puts "hi from tom"
  end
end
Tom.some_func
