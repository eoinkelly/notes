#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
import ExUnit.Assertions
import ExUnit.CaptureIO

alias Ecto.{Multi, Changeset}
alias MusicDB.{Repo, Artist, Genre}

Repo.insert!(%Artist{name: "Johnny Hodges"})

artist = Repo.get_by(Artist, name: "Johnny Hodges")
artist_changeset = Artist.changeset(artist,
  %{name: "John Cornelius Hodges"})
invalid_changeset = Artist.changeset(%Artist{},
  %{name: nil})
multi =
  Multi.new
  |> Multi.update(:artist, artist_changeset)
  |> Multi.insert(:invalid, invalid_changeset)
Repo.transaction(multi)
#=> {:error, :invalid,
#=>  #Ecto.Changeset<
#=>    action: :insert,
#=>    changes: %{},
#=>    errors: [name: {"can't be blank", [validation: :required]}],
#=>    data: #MusicDB.Artist<>,
#=>    valid?: false
#=>  >, %{}}

assert {:error, :invalid, %Changeset{}, %{}} = Repo.transaction(multi)

result = capture_io(fn ->
  case Repo.transaction(multi) do
    {:ok, _results} ->
      IO.puts "Operations were successful."
    {:error, :artist, changeset, _changes} ->
      IO.puts "Artist update failed"
      IO.inspect changeset.errors
    {:error, :invalid, changeset, _changes} ->
      IO.puts "Invalid operation failed"
      IO.inspect changeset.errors
  end
end)

assert String.match?(result, ~r{Invalid operation failed})

artist = Repo.get_by(Artist, name: "Johnny Hodges")
artist_changeset = Artist.changeset(artist,
  %{name: "John Cornelius Hodges"})
invalid_changeset = Artist.changeset(%Artist{},
  %{name: nil})
multi =
  Multi.new
  |> Multi.update(:artist, artist_changeset)
  |> Multi.insert(:invalid, invalid_changeset)
Repo.transaction(multi)
#=> {:error, :invalid,
#=>  #Ecto.Changeset<
#=>    action: :insert,
#=>    changes: %{},
#=>    errors: [name: {"can't be blank", [validation: :required]}],
#=>    data: #MusicDB.Artist<>,
#=>    valid?: false
#=>  >, %{}}

assert {:error, :invalid, %Changeset{}, %{}} = Repo.transaction(multi)

artist = Repo.get_by(Artist, name: "Johnny Hodges")
artist_changeset = Artist.changeset(artist,
  %{name: "John Cornelius Hodges"})
genre_changeset =
  %Genre{}
  |> Ecto.Changeset.cast(%{name: "jazz"}, [:name])
  |> Ecto.Changeset.unique_constraint(:name)
multi =
  Multi.new
  |> Multi.update(:artist, artist_changeset)
  |> Multi.insert(:bad_genre, genre_changeset)
Repo.transaction(multi)
#=> {:error, :bad_genre, #Ecto.Changeset< ... >,
#=> %{
#=>   artist: %MusicDB.Artist{
#=>     __meta__: #Ecto.Schema.Metadata<:loaded, "artists">,
#=>     albums: #Ecto.Association.NotLoaded<association
#=>       :albums is not loaded>,
#=>     birth_date: nil,
#=>     death_date: nil,
#=>     id: 4,
#=>     inserted_at: ~N[2018-03-23 14:02:28],
#=>     name: "John Cornelius Hodges",
#=>     tracks: #Ecto.Association.NotLoaded<association
#=>       :tracks is not loaded>,
#=>     updated_at: ~N[2018-03-23 14:02:28]
#=>   }
#=> }}

assert {:error, :bad_genre, %Ecto.Changeset{}, %{artist: %Artist{}}} = Repo.transaction(multi)

Repo.get_by(Artist, name: "John Cornelius Hodges")
#=> nil

assert nil == Repo.get_by(Artist, name: "John Cornelius Hodges")

multi =
  Multi.new
  |> Multi.insert(:artist, %Artist{})

assert %Multi{} = multi


if Repo.using_postgres?() do
  assert_raise(Postgrex.Error, fn ->
    Repo.transaction(multi)
    #=> ** (Postgrex.Error) ERROR 23502 (not_null_violation): null value
    #=>  in column "name" violates not-null constraint
  end)
else
  assert_raise(Mariaex.Error, fn ->
    Repo.transaction(multi)
  end)
end
