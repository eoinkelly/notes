defmodule ControlFlow do
  # I renamed do: to doit: from the book example to emphasise that it is not a
  # special block, just an atom i.e. `do:` is not `do`!
  # defmacro unless(expression, doit: block) do
    # maybe do: is a bit magic ??? - it seems to let me also provide a do ... end block to the macro which doit: does not ???
  defmacro unless(expression, do: block) do
    # expression, block are bindings created *outside* the quote
    # block so we need to use `unquote` macro to tell elixir that when we
    # type 'expression' within the quote block we actually mean that
    # binding not to generate a call to an 'expression' function
    quote do
      if ! unquote(expression), do: unquote(block)
    end
  end
end

# ex(19)> c "metaprogramming/unless.exs"
# [ControlFlow]
# iex(20)> require ControlFlow
# ControlFlow
# iex(21)> ControlFlow.unless 5 == 5, doit: IO.puts "hi"
# nil
# iex(22)> ControlFlow.unless 4 == 5, doit: IO.puts "hi"
# hi
# :ok


# iex(7)> ast = quote do
# ...(7)> ControlFlow.unless 4 == 5 do
# ...(7)> IO.puts "hi"
# ...(7)> end
# ...(7)> end
# {{:., [], [{:__aliases__, [alias: false], [:ControlFlow]}, :unless]}, [],
#  [{:==, [context: Elixir, import: Kernel], [4, 5]},
#   [do: {{:., [], [{:__aliases__, [alias: false], [:IO]}, :puts]}, [], ["hi"]}]]}
# iex(9)> expanded_once = Macro.expand_once ast, __ENV__
# {:if, [context: ControlFlow, import: Kernel],
#  [{:!, [context: ControlFlow, import: Kernel],
#    [{:==, [context: Elixir, import: Kernel], [4, 5]}]},
#   [do: {{:., [],
#      [{:__aliases__, [alias: false, counter: -576460752303423333], [:IO]},
#       :puts]}, [], ["hi"]}]]}
# iex(10)> expanded_fully = Macro.expand_once(expanded_once, __ENV__)
# {:case, [optimize_boolean: true],
#  [{:!, [context: ControlFlow, import: Kernel],
#    [{:==, [context: Elixir, import: Kernel], [4, 5]}]},
#   [do: [{:->, [],
#      [[{:when, [],
#         [{:x, [counter: -576460752303423325], Kernel},
#          {:in, [context: Kernel, import: Kernel],
#           [{:x, [counter: -576460752303423325], Kernel}, [false, nil]]}]}],
#       nil]},
#     {:->, [],
#      [[{:_, [], Kernel}],
#       {{:., [],
#         [{:__aliases__, [alias: false, counter: -576460752303423333], [:IO]},
#          :puts]}, [], ["hi"]}]}]]]}
