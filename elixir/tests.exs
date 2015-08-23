# Tests
# =====
# `mix test`
# * runs files matching `test/**/*_test.exs`
# * automatically loads test/test_helper.exs
# => always call test dir 'test'
# ExUnit.start
#
# # 2) Create a new test module (test case) and use [`ExUnit.Case`](ExUnit.Case.html).
# defmodule BuzzerTest do
#   # 3) Notice we pass `async: true`, this runs the test case
#   #    concurrently with other test cases
#   use ExUnit.Case, async: true
#
#   # 4) Use the `test` macro instead of `def` for clarity.
#   test "first two args are 0" do
#     assert buzzer.(0, 0, "foo") === "FizzBuzz"
#   end
# end

defmodule Buzzer do
  def buzz(0, 0, _), do: "FizzBuzz"
  def buzz(0, _, _), do: "Fizz"
  def buzz(_, 0, _), do: "Buzz"
  def buzz(_, _, a), do: a

  def fizz_buzz(n), do: buzz(rem(n, 3), rem(n, 5), n)
end

# `mix test`
# * runs files matching `test/**/*_test.exs`
# * automatically loads test/test_helper.exs
# => always call test dir 'test'
ExUnit.start

# 2) Create a new test module (test case)
defmodule BuzzerTest do
  # 3) Notice we pass `async: true`, this runs the test case
  #    concurrently with other test cases
  use ExUnit.Case, async: true

  # 4) Use the `test` macro instead of `def` for clarity.
  test "first two args are 0" do
    assert Buzzer.buzz(0, 0, "foo") === "FizzBuzz"
  end

  test "fizz_buzz works for 10-15" do
    assert Buzzer.fizz_buzz(10) === "Buzz"
    assert Buzzer.fizz_buzz(11) === 11
    assert Buzzer.fizz_buzz(12) === "Fizz"
    assert Buzzer.fizz_buzz(13) === 13
    assert Buzzer.fizz_buzz(14) === 14
    assert Buzzer.fizz_buzz(15) === "FizzBuzz"
  end
end
