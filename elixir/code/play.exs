# Use:
#
# iex> import_file "./play.exs"
#
# to load this into the main IEx context

fizzer = fn
  0, 0, _ -> "FizzBuzz"
  0, _, _ -> "Fizz"
  _, 0, _ -> "Buzz"
  _, _, n -> n
end

buzzer = fn
  n -> fizzer.(rem(n, 3), rem(n, 5), n)
end

# 10..15 |> Enum.each(fn i -> IO.inspect(buzzer.(i)) end)

prefix = fn
  first ->
    fn
      second -> "#{first} #{second}"
    end
end

hello_prefixed = prefix.("Hi!")
# IO.inspect(hello_john = hello_prefixed.("John"))
