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

albums_by_miles = from a in "albums",
  join: ar in "artists", on: a.artist_id == ar.id,
  where: ar.name == "Miles Davis"

assert %Query{} = albums_by_miles

q = from [a,ar] in albums_by_miles,
  where: ar.name == "Bobby Hutcherson",
  select: a.title

Repo.to_sql(:all, q)
#=> {"SELECT a0.\"title\" FROM \"albums\" AS a0
#=> INNER JOIN \"artists\" AS a1
#=> ON a0.\"artist_id\" = a1.\"id\"
#=> WHERE (a1.\"name\" = 'Miles Davis')
#=> AND (a1.\"name\" = 'Bobby Hutcherson')", []}

assert {_sql, []} = Repo.to_sql(:all, q)

q = from a in "albums",
  join: ar in "artists", on: a.artist_id == ar.id,
  where: ar.name == "Miles Davis" or ar.name == "Bobby Hutcherson",
  select: %{artist: ar.name, album: a.title}

assert %Query{} = q

q = from [a,ar] in albums_by_miles, or_where: ar.name == "Bobby Hutcherson",
  select: %{artist: ar.name, album: a.title}

assert %Query{} = q

q = from [a,ar] in albums_by_miles, or_where: ar.name == "Bobby Hutcherson",
  select: %{artist: ar.name, album: a.title}
Repo.all(q)
#=> [%{album: "Kind Of Blue", artist: "Miles Davis"},
#=>  %{album: "Cookin' At The Plugged Nickel", artist: "Miles Davis"},
#=>  %{album: "Live At Montreaux", artist: "Bobby Hutcherson"}]

assert MapSet.new([%{album: "Kind Of Blue", artist: "Miles Davis"},
  %{album: "Cookin' At The Plugged Nickel", artist: "Miles Davis"},
  %{album: "Live At Montreaux", artist: "Bobby Hutcherson"}]) == MapSet.new(Repo.all(q))


