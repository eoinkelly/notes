#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
defmodule IntervalExtension do
  @behaviour Postgrex.Extension

  def init(_opts), do: nil

  def matching(_state), do: [send: "interval_send"]

  def format(_state), do: :binary

  def encode(_state) do
    quote do
      {months, days, seconds} ->
        microseconds = seconds * 1_000_000
        <<16::32, microseconds::64, days::32, months::32>>
    end
  end

  def decode(_state) do
    quote do
      <<16::32, microseconds::64, days :: int32, months :: int32>> ->
        seconds = div(microseconds, 1_000_000)
        {months, days, seconds}
    end
  end
end

Postgrex.Types.define(
  MyApp.PostgrexTypes,
  [IntervalExtension] ++ Ecto.Adapters.Postgres.extensions(),
  json: Jason
)

_ = """
config :my_app, MyApp.Repo,
  types: MyApp.PostgrexTypes,
  #...
"""


