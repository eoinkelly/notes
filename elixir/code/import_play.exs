defmodule A do
  def say_hi do
    IO.puts("hi")
  end

  def say_bye do
    IO.puts("bye")
  end

  defmacro macro_if(clause, do: expression) do
    quote do
      if(unquote(clause), do: unquote(expression))
    end
  end
end

defmodule B do
  # Note that even though we only imported the say_hi function we got an
  # implicit `require A` which made the macro available to us under the `A`
  # namespace
  # import A, only: [say_hi: 0]

  # Note that even though we only imported functions we got an
  # implicit `require A` which made the macro available to us under the `A`
  # namespace
  import A, only: :functions

  def do_thing do
    say_hi()

    A.macro_if true do
      IO.puts("Should be printed")
    end
  end
end

B.do_thing()
