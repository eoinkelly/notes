
defmodule Hewey do
  def quack do
    IO.puts "Hewey quack"
  end
end

defmodule Dewey do
  alias Hewey, as: Hew # works at module level

  def squawk do
    alias Hewey, as: Hew # works at function level
    IO.puts "Dewey squawk"
    IO.puts Hewey.quack # works
    IO.puts Hew.quack # works
  end
end

# Dewey.squawk

defmodule Pinky do
  def pub, do: "public"
  def pub2, do: "public 2"
  defp priv, do: "private"
end

defmodule Brain do
  import Pinky, except: [pub2: 0]

  def show do
    IO.puts pub
    # IO.puts priv # private methods are not imported
  end
end

# Brain.show

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
