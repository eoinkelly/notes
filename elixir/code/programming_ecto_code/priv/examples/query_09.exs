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

q = from t in "tracks", join: a in "albums", on: t.album_id == a.id

assert %Query{} = q

q = from t in "tracks",
  join: a in "albums", on: t.album_id == a.id,
  where: t.duration > 900,
  select: [a.title, t.title]
Repo.all(q)
#=> [["Cookin' At The Plugged Nickel", "No Blues"],
#=> ["Cookin' At The Plugged Nickel", "If I Were A Bell"]]

assert MapSet.new([["Cookin' At The Plugged Nickel", "No Blues"],
  ["Cookin' At The Plugged Nickel", "If I Were A Bell"]]) == MapSet.new(Repo.all(q))

q = from t in "tracks",
  join: a in "albums", on: t.album_id == a.id,
  where: t.duration > 900,
  select: %{album: a.title, track: t.title}
Repo.all(q)
#=> [%{album: "Cookin' At The Plugged Nickel", track: "No Blues"},
#=> %{album: "Cookin' At The Plugged Nickel", track: "If I Were A Bell"}]

assert MapSet.new([%{album: "Cookin' At The Plugged Nickel", track: "No Blues"},
  %{album: "Cookin' At The Plugged Nickel", track: "If I Were A Bell"}]) == MapSet.new(Repo.all(q))

q = from t in "tracks", prefix: "public",
  join: a in "albums", prefix: "private",
  on: t.album_id == a.id, where: t.duration > 900,
  select: %{album: a.title, track: t.title}

assert %Query{} = q


q = from t in "tracks",
  join: a in "albums", on: t.album_id == a.id,
  join: ar in "artists", on: a.artist_id == ar.id,
  where: t.duration > 900,
  select: %{album: a.title, track: t.title, artist: ar.name}
Repo.all(q)
#=> [%{album: "Cookin' At The Plugged Nickel", artist: "Miles Davis",
#=> track: "If I Were A Bell"},
#=> %{album: "Cookin' At The Plugged Nickel", artist: "Miles Davis",
#=> track: "No Blues"}]

assert MapSet.new([%{album: "Cookin' At The Plugged Nickel", artist: "Miles Davis",
  track: "If I Were A Bell"}, %{album: "Cookin' At The Plugged Nickel",
  artist: "Miles Davis", track: "No Blues"}]) == MapSet.new(Repo.all(q))
