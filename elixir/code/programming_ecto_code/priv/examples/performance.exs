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

alias MusicDB.{Repo, Album, Artist, Track}

query =
from a in Album,
  join: t in assoc(a, :tracks),
  join: ar in assoc(a, :artist),
  preload: [tracks: t, artist: ar]

result = Repo.all(query)
assert Enum.count(result) == 5
assert Enum.all?(result, fn a -> Enum.count(a.tracks) > 0 end)
assert Enum.all?(result, fn a -> !is_nil(a.artist) end)


query =
# Preload with atoms or keyword
from a in Album, preload: [:tracks]
# Preload with atoms or keyword

assert %Ecto.Query{} = query


# Preload with anonymous functions
track_fun = fn album_ids ->
  Repo.all(from(t in Track, where: t.album_id in ^album_ids))
end
Repo.all(from(a in Album, preload: [tracks: ^track_fun]))

result = Repo.all(from(a in Album, preload: [tracks: ^track_fun]))
assert Enum.count(result) == 5
assert Enum.all?(result, fn a -> Enum.count(a.tracks) > 0 end)


# Using Repo.preload
albums = Repo.all(Album)
Repo.preload(albums, [:tracks])

result = Repo.preload(albums, [:tracks])
assert Enum.count(result) == 5
assert Enum.all?(result, fn a -> Enum.count(a.tracks) > 0 end)
assert Enum.all?(result, fn a -> !is_nil(a.artist) end)


q = from t in Track, select: [:title, :duration]
Repo.all(q)

assert Enum.count(Repo.all(q)) == 33

tracks = Repo.all(Track)
Enum.each(tracks, fn track ->
  track
  |> Ecto.Changeset.change(%{number_of_plays: 0})
  |> Repo.update!()
end)


Repo.update_all(Track, set: [number_of_plays: 0])


current_artist_count = Repo.aggregate(Artist, :count, :id)
artist_records = [
  %{
    "name" => "Shirley Horn",
    "birth_date" => ~D[1934-05-01],
    "death_date" => ~D[2005-10-20]
  }
]

artists =
  Enum.map(artist_records, fn artist ->
    %{name: artist["name"],
      birth_date: artist["birth_date"],
      death_date: artist["death_date"]}
  end)

Repo.insert_all(Artist, artists)

assert Repo.aggregate(Artist, :count, :id) == current_artist_count + 1


chunks = Enum.chunk_every(artist_records, 1000)
Enum.each(chunks, fn chunk ->
  artists_chunk =
    Enum.map(chunk, fn artist ->
      %{name: artist["name"],
        birth_date: artist["birth_date"],
        death_date: artist["death_date"]}
    end)
  Repo.insert_all(Artist, artists_chunk)
end)

assert Repo.aggregate(Artist, :count, :id) == current_artist_count + 2


defmodule MusicDB.StreamExample do

  def save_artist_record(_artist) do
    # no-op
  end

  def stream_example do
    stream =
      Artist
      |> Repo.stream()
      |> Task.async_stream(fn artist ->
        save_artist_record(artist)
      end)

    Repo.transaction(fn ->
      Stream.run(stream)
    end)
  end
end

MusicDB.StreamExample.stream_example()


query = from(Artist, order_by: [:id])
chunk_size = 500
offset = 0

stream =
  Stream.resource(
    fn -> 0 end,
    fn
      :stop -> {:halt, :stop}
      offset ->
        rows =
          Repo.all(from(query, limit: ^chunk_size, offset: ^offset))
        if Enum.count(rows) < chunk_size do
          {rows, :stop}
        else
          {rows, offset + chunk_size}
        end
    end,
    fn _ -> :ok end
  )

Stream.run(stream)
