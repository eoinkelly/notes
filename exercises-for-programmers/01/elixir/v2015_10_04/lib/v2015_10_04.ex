defmodule V2015_10_04 do
  @prompt "What is your name? "

  # $ mix escript.build
  # creates ./V2015_10_04
  def main(_args) do
    echo
  end

  def echo do
    name = IO.gets(@prompt) |> String.strip
    IO.puts "Hello, #{name}, nice to meet you!"
  end
end
