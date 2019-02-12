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

assert_raise(UndefinedFunctionError, fn ->
params = %{"name" => "Esperanza Spalding",
  "albums" => [%{"title" => "Junjo"}]}
changeset =
  %Artist{}
  |> cast(params, [:name])
  |> cast_assoc(:albums)
changeset.changes
end)

defmodule MusicDB.Album do
  use Ecto.Schema
  import Ecto.Changeset
  alias MusicDB.{Artist, Track, Genre}

  schema "albums" do
    field(:title, :string)
    timestamps()

    belongs_to(:artist, Artist)
  end

  # add this to lib/music_db/album.ex
  def changeset(album, params) do
    album
    |> cast(params, [:title])
    |> validate_required([:title])
  end
end

params = %{"name" => "Esperanza Spalding",
  "albums" => [%{"title" => "Junjo"}]}
changeset =
  %Artist{}
  |> cast(params, [:name])
  |> cast_assoc(:albums)
changeset.changes
#=> %{albums: [#Ecto.Changeset<action: :insert,
#=>  changes: %{title: "Junjo"}, errors: [],
#=>  data: #MusicDB.Album<>, valid?: true>],
#=>  name: "Esperanza Spalding"}

assert %{albums: [%Ecto.Changeset{}], name: "Esperanza Spalding"} = changeset.changes

artist = Repo.get_by(Artist, name: "Bill Evans")
  |> Repo.preload(:albums)
IO.inspect Enum.map(artist.albums, &({&1.id, &1.title}))
#=> [{4, "Portrait In Jazz"}, {3, "You Must Believe In Spring"}]

assert MapSet.new([{4, "Portrait In Jazz"}, {3, "You Must Believe In Spring"}]) ==
  MapSet.new(Enum.map(artist.albums, &({&1.id, &1.title})))

portrait = Repo.get_by(Album, title: "Portrait In Jazz")
kind_of_blue = Repo.get_by(Album, title: "Kind Of Blue")
params = %{"albums" =>
  [
    %{"title" => "Explorations"},
    %{"title" => "Portrait In Jazz (remastered)", "id" => portrait.id},
    %{"title" => "Kind Of Blue", "id" => kind_of_blue.id}
  ]
}

defmodule MusicDB.Artist do
  use Ecto.Schema
  import Ecto.Changeset
  alias MusicDB.{Artist, Album}

  schema "artists" do
    field(:name)
    field(:birth_date, :date)
    field(:death_date, :date)
    timestamps()

    has_many(:albums, Album, on_replace: :nilify)
    has_many(:tracks, through: [:albums, :tracks])
  end

  def changeset(artist, params) do
    artist
    |> cast(params, [:name, :birth_date, :death_date])
    |> validate_required([:name])
  end
end

portrait = Repo.get_by(Album, title: "Portrait In Jazz")
kind_of_blue = Repo.get_by(Album, title: "Kind Of Blue")
params = %{"albums" =>
  [
    %{"title" => "Explorations"},
    %{"title" => "Portrait In Jazz (remastered)", "id" => portrait.id},
    %{"title" => "Kind Of Blue", "id" => kind_of_blue.id}
  ]
}

artist = Repo.get_by(Artist, name: "Bill Evans")
  |> Repo.preload(:albums)
{:ok, artist} =
  artist
  |> cast(params, [])
  |> cast_assoc(:albums)
  |> Repo.update
Enum.map(artist.albums, &({&1.id, &1.title}))
#=> [{6, "Explorations"}, {4, "Portrait In Jazz (remastered)"},
#=>  {7, "Kind Of Blue"}]

assert MapSet.new([{6, "Explorations"}, {4, "Portrait In Jazz (remastered)"},
  {7, "Kind Of Blue"}]) == MapSet.new(Enum.map(artist.albums, &({&1.id, &1.title})))


result =
Repo.all(from a in Album, where: a.title == "Kind Of Blue")
|> Enum.map(&({&1.id, &1.title}))
#=> [{1, "Kind Of Blue"}, {7, "Kind Of Blue"}]

assert MapSet.new([{1, "Kind Of Blue"}, {7, "Kind Of Blue"}]) == MapSet.new(result)

result =
Repo.all(from a in Album, where: a.title == "You Must Believe In Spring")
|> Enum.map(&({&1.id, &1.title, &1.artist_id}))
#=> [{3, "You Must Believe In Spring", nil}]

assert MapSet.new([{3, "You Must Believe In Spring", nil}]) == MapSet.new(result)

