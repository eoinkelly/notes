defmodule PrettyAst do
  @colors [:white, :red, :yellow, :green, :blue, :cyan, :magenta]

  @doc """
  Pretty prints the given AST to the screen

  See the docs for PrettyAst.pretty_print for details.
  """
  def puts(ast) do
    pretty_print(ast) |> IO.puts
  end

  @doc """
  Converts the given AST to an ANSI colorized string.

  * Assumes Elixir AST structure i.e. three tuples
  * Each nested tuple gets a new color to help distinguish levels within the AST
  * Color palette is limited so is recycled if nesting is sufficiently deep
  """
  def pretty_print(ast) do
    do_pretty_print(ast, 0)
  end

  defp do_pretty_print(list, level) when is_list(list) do
    open = colorize("[", level)
    close = colorize("]", level)
    sep = colorize(", ", level)

    contents = list |> Enum.map(fn el -> do_pretty_print(el, level) end)
                    |> Enum.join(sep)

   "#{open}#{contents}#{close}"
  end

  defp do_pretty_print(tuple, level) when is_tuple(tuple) do
    new_level = level + 1 # tuples get a new color

    open = colorize("{", new_level)
    close = colorize("}", new_level)
    sep = colorize(", ", new_level)

    contents = tuple |> Tuple.to_list
                     |> Enum.map(fn el -> do_pretty_print(el, new_level) end)
                     |> Enum.join(sep)

   "#{open}#{contents}#{close}"
  end

  defp do_pretty_print(any, level) do
    colorize("#{inspect any}", level)
  end

  defp colorize(content, level) do
    "#{IO.ANSI.format([color(level), content])}"
  end

  # Cycle through the list of colors
  defp color(level) do
    Enum.at(@colors, rem(level, length(@colors)))
  end
end
