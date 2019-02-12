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
alias MusicDB.Repo

q = from "artists", where: [name: "Bill Evans"], select: [:id, :name]
Repo.all(q)
#=> [%{id: 2, name: "Bill Evans"}]

assert [%{name: "Bill Evans"}] = Repo.all(q)

assert_raise(Ecto.Query.CompileError, fn ->
  ast = quote do

    artist_name = "Bill Evans"
    q = from "artists", where: [name: artist_name], select: [:id, :name]

  end
  Code.eval_quoted(ast, [], __ENV__)
end)

artist_name = "Bill Evans"
q = from "artists", where: [name: ^artist_name], select: [:id, :name]
#=> #Ecto.Query<from a in "artists", where: a.name == ^"Bill Evans",
#=> select: [:id, :name]>

assert %Ecto.Query{} = q
