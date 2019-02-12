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

q = from a in "artists", select: [a.name], order_by: a.name
Repo.all(q)
#=> [["Bill Evans"], ["Bobby Hutcherson"], ["Miles Davis"]]

assert [["Bill Evans"], ["Bobby Hutcherson"], ["Miles Davis"]] = Repo.all(q)

q = from a in "artists", select: [a.name], order_by: [desc: a.name]
Repo.all(q)
#=> [["Miles Davis"], ["Bobby Hutcherson"], ["Bill Evans"]]

assert [["Miles Davis"], ["Bobby Hutcherson"], ["Bill Evans"]] = Repo.all(q)

q = from t in "tracks", select: [t.album_id, t.title, t.index],
  order_by: [t.album_id, t.index]
Repo.all(q)
#=> [[1, "So What", 1], [1, "Freddie Freloader", 2], [1, "Blue In Green", 3],
#=> [1, "All Blues", 4], [1, "Flamenco Sketches", 5],
#=> [2, "If I Were A Bell", 1], [2, "Stella By Starlight", 2],
#=> [2, "Walkin'", 3], [2, "Miles", 4], [2, "No Blues", 5], ... ]

assert [[1, "So What", 1] | _tail] = Repo.all(q)

q = from t in "tracks", select: [t.album_id, t.title, t.index],
  order_by: [desc: t.album_id, asc: t.index]
Repo.all(q)
#=> [[5, "Anton's Ball", 1], [5, "The Moontrane", 2], [5, "Farallone", 3],
#=> [5, "Song Of Songs", 4], [4, "Come Rain Or Come Shine", 1], ... ]

assert [[5, "Anton's Ball", 1] | _tail] = Repo.all(q)

if Repo.using_postgres?() do
  q = from t in "tracks", select: [t.album_id, t.title, t.index],
    order_by: [desc: t.album_id, asc_nulls_first: t.index]
  Repo.all(q)
  #=> [[5, "Anton's Ball", 1], [5, "The Moontrane", 2], [5, "Farallone", 3],
  #=> [5, "Song Of Songs", 4], [4, "Come Rain Or Come Shine", 1], ... ]
end

assert [[5, "Anton's Ball", 1] | _tail] = Repo.all(q)

q = from t in "tracks", select: [t.album_id, t.title, t.index],
  order_by: [t.index, t.album_id]
Repo.all(q)
#=> [[1, "So What", 1], [2, "If I Were A Bell", 1],
#=> [3, "B Minor Waltz (for Ellaine)", 1], [4, "Come Rain Or Come Shine", 1],
#=> [5, "Anton's Ball", 1], [1, "Freddie Freloader", 2],
#=> [2, "Stella By Starlight", 2], [3, "You Must Believe In Spring", 2],
#=> [4, "Autumn Leaves", 2], [5, "The Moontrane", 2], ...

assert [[1, "So What", 1] | _tail] = Repo.all(q)

q = from t in "tracks", select: [t.album_id, sum(t.duration)],
  group_by: t.album_id

assert %Query{} = q

Repo.all(q)
#=> [[4, 2540], [1, 2619], [5, 3057], [3, 3456], [2, 4491]]

if Repo.using_postgres?() do
  assert MapSet.new([[4, 2540], [1, 2619], [5, 3057], [3, 3456], [2, 4491]]) == MapSet.new(Repo.all(q))
end

q = from t in "tracks", select: [t.album_id, sum(t.duration)],
  group_by: t.album_id,
having: sum(t.duration) > 3600
Repo.all(q)
#=> [[2, 4491]]

if Repo.using_postgres?() do
  assert [[2, 4491]] = Repo.all(q)
end

tracks_query = from t in "tracks", select: t.title
union_query = from a in "albums", select: a.title,
  union: ^tracks_query
Repo.all(union_query)
#=> ["Without a Song", "Gary's Theme", "Miles", "Kind Of Blue", ...]

titles = ["Without a Song", "Gary's Theme", "Miles", "Kind Of Blue"]
result = Repo.all(union_query)
assert Enum.all?(titles, fn title -> title in result end)

tracks_query = from t in "tracks", select: t.title
union_query = from a in "albums", select: a.title,
  union_all: ^tracks_query
Repo.all(union_query)
#=> ["Without a Song", "Gary's Theme", "Miles", "Kind Of Blue", ...]

result = Repo.all(union_query)
assert Enum.all?(titles, fn title -> title in result end)

tracks_query = from t in "tracks", select: t.title
intersect_query = from a in "albums", select: a.title,
  intersect: ^tracks_query

assert %Query{} = intersect_query
assert ["You Must Believe In Spring"] == Repo.all(intersect_query)

tracks_query = from t in "tracks", select: t.title
except_query = from a in "albums", select: a.title,
  except: ^tracks_query

assert "You Must Believe In Spring" not in Repo.all(except_query)



