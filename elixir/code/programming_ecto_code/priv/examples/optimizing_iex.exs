#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
import ExUnit.Assertions
import IEx.Helpers

alias MusicDB.{
  Repo,
  Artist,
  Album,
  Track,
  Genre,
  Log
}

import_if_available Ecto.Query

import_if_available Ecto.Query, only: [from: 2]

import_if_available Ecto.Changeset

defmodule H do

  def update(schema, changes) do
    schema
    |> Ecto.Changeset.change(changes)
    |> Repo.update
  end

end

result = (fn ->
  artist = Repo.get_by(Artist, name: "Miles Davis")
  H.update(artist, name: "Miles Dewey Davis III",
    birth_date: ~D[1926-05-26])
  #=> {:ok,
  #=> %MusicDB.Artist{
  #=>   __meta__: #Ecto.Schema.Metadata<:loaded, "artists">,
  #=>   albums: #Ecto.Association.NotLoaded<association :albums is not loaded>,
  #=>   birth_date: ~D[1926-05-26],
  #=>   ...
  #=>   name: "Miles Dewey Davis III",
  #=>   ...
  #=> }}
end).()

assert {:ok, %MusicDB.Artist{name: "Miles Dewey Davis III"}} = result

defmodule H do

  #...

  def load_album(id) do
    Repo.get(Album, id) |> Repo.preload(:tracks)
  end

end

defmodule H do

  #...

  def load_album(title) when is_binary(title) do
    Repo.get_by(Album, title: title) |> Repo.preload(:tracks)
  end

  def load_album(id) do
    Repo.get(Album, id) |> Repo.preload(:tracks)
  end

end

assert %MusicDB.Album{title: "Kind Of Blue"} = H.load_album("Kind Of Blue")
assert %MusicDB.Album{title: "Kind Of Blue"} = H.load_album(1)

