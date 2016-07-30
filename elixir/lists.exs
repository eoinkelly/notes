# These examples from chapter 7

# [head | tail] can be used to construct a list
# [head | tail] can be used to split a list

defmodule EoinList do
  def map([], _fun), do: []
  def map([head | tail], fun) do # <-- splitting
    [fun.(head) | map(tail, fun)] # <-- construction
  end

  # common pattern to have a public function with a nice interface that calls
  # the private function which has the extra param required to be the
  # accumulator
  def sum(list) do
    # _sum(list, 0) # option 1: has accumulator
    do_sum(list) # option 2: no accumulator, result is accumulated in return value
  end

  # defp _sum([], total), do: total
  # defp _sum([h | t], total) do
  #   _sum(t, total + h)
  # end

  # `do_sum` is also a popular name for this func
  defp do_sum([]), do: 0
  defp do_sum([head | tail]) do
    head + do_sum(tail)
  end

  def mapsum(list, fun) do
    sum(map(list, fun))
  end

  # def span(from, to) when from == to, do: [to]
  def span(to, to), do: [to]
  def span(from, to) do
    [from |  span(from + 1, to)]
  end

  def max([head | tail]) do
    # set intial maximum to head
    do_max(tail, head)
  end

  # return the current max if we reach end of list
  defp do_max([], max), do: max

  # set maximum to head if head greater than current maximum
  defp do_max([head|tail], max) when head > max do
    do_max(tail, head)
  end

  # otherwise continue thorough the list.
  defp do_max([_head|tail], max) do
    do_max(tail, max)
  end

  # assume all chars are in a-z range

  # stop when we reach the end of the list
  def caesar([], _n), do: []

  # in cases where we wrap ...
  def caesar([head | tail], n) when (head + n) > ?z do
    [offset + overshoot(head, n) | caesar(tail, n)]
  end

  # otherwise ...
  def caesar([head | tail], n) do
    [head + n | caesar(tail, n)]
  end

  defp offset do
    ?a - 1
  end

  defp overshoot(head, n) do
    (head + n) - ?z
  end
end

things = [2,4,5,6,7,9,4,5,7]

IO.inspect EoinList.map things, fn (x) -> x*3+1 end
IO.inspect EoinList.map things, &(&1 * 3 + 1) # same as above
IO.inspect EoinList.sum(things)

IO.inspect EoinList.span(11, 23)
IO.inspect EoinList.max(things)
IO.inspect EoinList.mapsum(things, &(&1 + 1))

IO.inspect EoinList.caesar('ryvkve', 13)
