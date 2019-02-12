#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
import ExUnit.Assertions

alias MusicDB.{Repo, Artist, Album, Note}

defmodule MusicDB.Artist do
  use Ecto.Schema
  import Ecto.Changeset
  alias MusicDB.{Artist, Album}

  schema "artists" do
    field(:name)
    has_many(:albums, Album)
    has_many(:tracks, through: [:albums, :tracks])

    has_many :notes, MusicDB.Note
  end
end

result = (fn ->
  artist = Repo.get_by(Artist, name: "Bobby Hutcherson")
  note = Ecto.build_assoc(artist, :notes,
    note: "My fave vibes player", author: "darin")
  Repo.insert!(note)
  artist = Repo.preload(artist, :notes)
  artist.notes
  # => [
  #  %MusicDB.Note{
  #    ...
  #  }
  #]
end).()

assert [%Note{}] = result

defmodule MusicDB.Album do
  use Ecto.Schema

  schema "albums" do
    field(:title, :string)
    has_many :notes, MusicDB.Note
  end
end

result = (fn ->
  album = Repo.get_by(Album, title: "Kind Of Blue")
  note = Ecto.build_assoc(album, :notes,
    note: "Love this album!", author: "darin")
  Repo.insert!(note)
  album = Repo.preload(album, :notes)
  album.notes
  # => [
  #  %MusicDB.Note{
  #    ...
  #  }
  #]
end).()

assert [%Note{}] = result
