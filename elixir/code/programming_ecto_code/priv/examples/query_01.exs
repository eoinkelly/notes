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

_ = """
SELECT t.id, t.title, a.title
  FROM tracks t
  JOIN albums a ON t.album_id = a.id
  WHERE t.duration > 900;
"""

query = from t in "tracks",
  join: a in "albums", on: t.album_id == a.id,
  where: t.duration > 900,
  select: [t.id, t.title, a.title]

assert %Ecto.Query{} = query

query = "tracks"
|> join(:inner, [t], a in "albums", on: t.album_id == a.id)
|> where([t,a], t.duration > 900)
|> select([t,a], [t.id, t.title, a.title])

assert %Ecto.Query{} = query

query = from "artists", select: [:name]
#=> #Ecto.Query<from a in "artists", select: [:name]>

assert %Ecto.Query{} = query

query = from "artists", select: [:name]
Ecto.Adapters.SQL.to_sql(:all, Repo, query)
#=> {"SELECT a0.\"name\" FROM \"artists\" AS a0", []}

query = from "artists", select: [:name]
Repo.to_sql(:all, query)

query = from "artists", select: [:name]
Repo.all(query)
#=> [%{name: "Miles Davis"}, %{name: "Bill Evans"},
#=> %{name: "Bobby Hutcherson"}]

assert [%{}, %{}, %{}] = Repo.all(query)

query = Ecto.Query.from("artists", select: [:name])

assert %Ecto.Query{} = query

assert_raise(Ecto.QueryError, fn ->

query = from "artists"
Repo.all(query)
#=> ** (Ecto.QueryError) ...

end)

query = from "artists", prefix: "public", select: [:name]

assert %Ecto.Query{} = query
