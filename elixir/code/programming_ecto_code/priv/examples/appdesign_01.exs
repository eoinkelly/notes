#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
import ExUnit.Assertions

# lib/music_db/music/artist.ex
defmodule MusicDB.Music.Artist do
  use Ecto.Schema

  schema "artists" do
    field :name, :string
    has_many :albums, MusicDB.Music.Album
  end
end

# lib/music_db/music/album.ex
defmodule MusicDB.Music.Album do
  use Ecto.Schema
  import Ecto.Query
  alias MusicDB.Music.{Album, Artist}

  schema "albums" do
    field :title, :string
    belongs_to :artist, Artist
  end

  def search(string) do
    from album in Album,
      where: ilike(album.title, ^"%#{string}%")
   end
end

# lib/music_db/music.ex
defmodule MusicDB.Music do
  alias MusicDB.Music.{Repo, Album, Artist}

  def get_artist(name) do
    MusicDB.Repo.get_by(Artist, name: name)
  end

  def all_albums_by_artist(artist) do
    Ecto.assoc(artist, :albums)
    |> MusicDB.Repo.all()
  end

  def search_albums(string) do
    string
    |> Album.search()
    |> MusicDB.Repo.all()
  end
end

if MusicDB.Repo.using_postgres?() do
  artist = MusicDB.Music.get_artist("Miles Davis")
  assert "Miles Davis" = artist.name
  assert 2 = Enum.count(MusicDB.Music.all_albums_by_artist(artist))
  assert "Cookin' At The Plugged Nickel" = hd(MusicDB.Music.search_albums("nickel")).title
end

