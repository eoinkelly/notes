#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
import ExUnit.Assertions

import Ecto.Changeset
import Ecto.Query
alias MusicDB.{Repo, Artist, Album}

## The first two examples are commented out because they won't actually run

assert_raise(RuntimeError, fn ->
changeset = Repo.get_by(Artist, name: "Miles Davis")
  |> change
  |> put_assoc(:albums, [%Album{title: "Miles Ahead"}])
Repo.update(changeset)
#=> ** (RuntimeError) attempting to cast or change association `albums`
#=> from `MusicDB.Artist` that was not loaded. Please preload your
#=> associations before manipulating them through changesets
end)

assert_raise(RuntimeError, fn ->
changeset = Repo.get_by(Artist, name: "Miles Davis")
  |> Repo.preload(:albums)
  |> change
  |> put_assoc(:albums, [%Album{title: "Miles Ahead"}])
Repo.update(changeset)
#=> ** (RuntimeError) you are attempting to change relation :albums of
#=> MusicDB.Artist but the `:on_replace` option of
#=> this relation is set to `:raise`.
#=> ...
end)

artist =
  Repo.get_by(Artist, name: "Miles Davis")
  |> Repo.preload(:albums)

artist
 |> change
 |> put_assoc(:albums, [%Album{title: "Miles Ahead"} | artist.albums])
 |> Repo.update

assert %Album{title: "Miles Ahead", artist_id: 1} = Repo.get_by(Album, title: "Miles Ahead")

%Artist{name: "Eliane Elias"}
|> change
|> put_assoc(:albums, [%Album{title: "Made In Brazil"}])
|> Repo.insert

# adding an album with a map
%Artist{name: "Eliane Elias"}
|> change
|> put_assoc(:albums, [%{title: "Made In Brazil"}])
|> Repo.insert

# adding an album with a keyword list
%Artist{name: "Eliane Elias"}
|> change
|> put_assoc(:albums, [[title: "Made In Brazil"]])
|> Repo.insert

q = from a in Artist, where: a.name == "Eliane Elias"
assert 3 == Repo.aggregate(q, :count, :id)

q = from a in Album, where: a.title == "Made In Brazil"
assert 3 == Repo.aggregate(q, :count, :id)
