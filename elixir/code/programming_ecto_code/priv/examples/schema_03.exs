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

q = from a in "artists",
  join: al in "albums",
  on: a.id == al.artist_id,
  group_by: a.name,
  select: %{artist: a.name, number_of_albums: count(al.id)}
Repo.all(q)
#=> [%{artist: "Miles Davis", number_of_albums: 2},
#=> %{artist: "Bobby Hutcherson", number_of_albums: 1},
#=> %{artist: "Bill Evans", number_of_albums: 2}]

assert MapSet.new([%{artist: "Miles Davis", number_of_albums: 2},
  %{artist: "Bobby Hutcherson", number_of_albums: 1},
  %{artist: "Bill Evans", number_of_albums: 2}]) == MapSet.new(Repo.all(q))

