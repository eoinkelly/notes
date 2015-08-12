defmodule Chop do
  def guess(actual, range, guess \\ :no_guess)

  def guess(actual, range, :no_guess) do
    guess(actual, range, middle_of(range))
  end

  def guess(actual, _, guess) when actual == guess do
    IO.puts "Answer is #{guess}"
  end

  def guess(actual, low.._, guess) when actual < guess do
    IO.puts "Is it #{guess}?"
    guess(actual, low..guess, middle_of(low..guess))
  end

  def guess(actual, _..high, guess) when actual > guess do
    IO.puts "Is it #{guess}?"
    guess(actual, guess..high, middle_of(guess..high))
  end

  defp middle_of(low..high) do
    low + div((high - low), 2)
  end
end

Chop.guess(273, 1..1000)
