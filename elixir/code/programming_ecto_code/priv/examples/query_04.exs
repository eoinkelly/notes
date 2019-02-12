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

artist_id = 1
q = from "artists", where: [id: ^artist_id], select: [:name]
Repo.all(q)
#=> [%{name: "Miles Davis"}]

assert [%{name: "Miles Davis"}] = Repo.all(q)

if Repo.using_postgres?() do
  assert_raise(DBConnection.EncodeError, fn ->

    artist_id = "1"
    q = from "artists", where: [id: ^artist_id], select: [:name]
    Repo.all(q)
    #=> ** (DBConnection.EncodeError) Postgrex expected an integer
    #=> in -2147483648..2147483647 that can be encoded/cast to
    #=> type "int4", got "1". Please make sure the value you
    #=> are passing matches the definition in your table or
    #=> in your query or convert the value accordingly.

  end)
end

artist_id = "1"
q = from "artists", where: [id: type(^artist_id, :integer)], select: [:name]
Repo.all(q)
#=> [%{name: "Miles Davis"}]

assert [%{name: "Miles Davis"}] = Repo.all(q)
