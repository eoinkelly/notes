#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
import ExUnit.Assertions

import Ecto.Query
alias Ecto.Query
alias MusicDB.Repo

q = from a in "artists",
  where: fragment("lower(?)", a.name) == "miles davis",
  select: [:id, :name]

assert %Query{} = q

q = from a in "artists",
  where: fragment("lower(?)", a.name) == "miles davis",
  select: [:id, :name]
Ecto.Adapters.SQL.to_sql(:all, Repo, q)
#=> {"SELECT a0.\"id\", a0.\"name\" FROM \"artists\" AS a0
#=> WHERE (lower(a0.\"name\") = 'miles davis')", []}

assert {_sql, []} = Ecto.Adapters.SQL.to_sql(:all, Repo, q)

defmodule Chapter02Example do

  defmacro lower(arg) do
    quote do: fragment("lower(?)", unquote(arg))
  end

  def macro_example do
    q = from a in "artists",
      where: lower(a.name) == "miles davis",
      select: [:id, :name]
  end
end

assert %Query{} = Chapter02Example.macro_example()


