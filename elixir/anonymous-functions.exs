# Anonymous functions
# ###################

# definining functions and binding them to names
sum = fn (a, b) -> a + b end
sum2 = fn a, b -> a + b end
fun = fn -> 99 end

# Invoking functions notice the foo.(args) pattern
IO.puts sum.(5,3)
IO.puts sum2.(5,3)
IO.puts fun.() # parens *required* for anonymous functions

# File.open is an elixir module
# :file.format_error/1 is an Erlang function
# this is how you use Erlang stuff in Elixir
handle_open = fn
  {:ok, file} -> "Read data: #{IO.read(file, :line)}"
  {_, error} -> "Error: #{:file.format_error(error)}"
end

IO.puts handle_open.(File.open("./test.txt"))
IO.puts handle_open.(File.open("./notthere.txt"))


# Fizzbuzz exercise
# =================

# “Write a function that takes three arguments. If the first two are zero,
# return “FizzBuzz.” If the first is zero, return “Fizz.” If the second is
# zero, return “Buzz.” Otherwise return the third argument”

# note: no static types for args or return value!
buzzer = fn
  0, 0, _ -> "FizzBuzz"
  0, _, _ -> "Fizz"
  _, 0, _ -> "Buzz"
  _, _, a -> a
end


IO.puts "Fizz buzzing ================"
IO.puts buzzer.(0, 0, 1) # expect "FizzBuzz"
IO.puts buzzer.(0, 2, 1) # expect "Fizz"
IO.puts buzzer.(1, 0, 3) # expect "Buzz"
IO.puts buzzer.(1, 3, "hello") # expect "hello"

# “Write a function that takes a single integer (n) and calls the function in the
# previous exercise, passing it rem(n,3), rem(n,5), and n. Call it seven times
# with the arguments 10, 11, 12, and so on. You should get “Buzz, 11, Fizz, 13,
# 14, FizzBuzz, 16.”

fizz_buzz = fn
  n -> buzzer.(rem(n, 3), rem(n, 5), n)
end

fizz_buzz.(10)
IO.puts fizz_buzz.(10)
IO.puts fizz_buzz.(11)
IO.puts fizz_buzz.(12)
IO.puts fizz_buzz.(13)
IO.puts fizz_buzz.(14)
IO.puts fizz_buzz.(15)
IO.puts fizz_buzz.(16)

# # Closures
#
# * elixir has lexical closures that work pretty much how you expect -
#   functions can return other functions and will keep access to all the
#   variables bound in the lexical scope that the function was defined in.
adder = fn a -> (fn b -> a + b end) end
add_two = adder.(2)
IO.puts add_two.(4)
IO.puts adder.(10).(30)

# “Write a function prefix that takes a string. It should return a new function
# that takes a second string. When that second function is called, it will return
# a string containing the first string, a space, and the second string.”

concatter = fn str -> (fn str2 -> "#{str} #{str2}" end) end
IO.puts concatter.("hello").("world")

# functions can be passed as args
apply = fn func, val -> func.(val) end
IO.puts apply.(add_two, 55)

# & the "function capture" operator
# =================================

doubler_1 = fn x -> x * 2 end
doubler_2 = &(&1 * 2)
# & operator

# Uses:
#
# 1. Alias existing functions with no runtime cost
# 2. Nice for quick inline lambdas
#
# * causes the *expression* that follows to be wrapped in an anonymous function
# * has 2 forms:
#     &<EXPRESSION>
#     &Module.func/arity
#
#   &(<STUFF>) becomes fn x -> <STUFF> end
#   every &1, &2 inside refers to positional args
# * Elixir will optimize away the wrapping function if it can

# you can make nice shorthands for existing methods
isp = &Kernel.inspect(&1)
isp = &Kernel.inspect/1 # same as above

list = [1,3,5,7,9]
IO.puts Kernel.inspect(Enum.map(list, doubler_2))
IO.puts isp.(Enum.map(list, doubler_2))
IO.puts Kernel.inspect(Enum.map(list, &(&1 * 2))) # short syntax nice for quick inline functions
IO.inspect Enum.map(list, &(&1 * 2)) # short syntax nice for quick inline functions

Enum.map [1,2,3,4], fn x -> x + 2 end
Enum.map [1,2,3,4], &(&1 + 2)
Enum.each [1,2,3,4], fn x -> IO.inspect x end
Enum.each [1,2,3,4], &IO.inspect/1


# blocks are used to create scopes in elixir
# their syntax is  do: (<STATEMENTS SEPARATED BY NEWLINES>)
# but the do..end syntax is more common

