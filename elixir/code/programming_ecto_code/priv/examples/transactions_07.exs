#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
import ExUnit.Assertions

alias Ecto.{Multi, Changeset}
alias MusicDB.{Repo, Artist, Log}

artist = %Artist{name: "Toshiko Akiyoshi"}
multi =
  Multi.new()
  |> Multi.insert(:artist, artist)
  |> Multi.insert(:log, Log.changeset_for_insert(artist))
  |> Multi.run(:search, fn _repo, changes ->
    SearchEngine.update(changes[:artist])
  end)
Repo.transaction(multi)

assert {:ok, %{artist: %Artist{}, log: %Log{}, search: %Artist{}}} = Repo.transaction(multi)

multi =
  Multi.new()
  |> Multi.insert(:artist, artist)
  |> Multi.insert(:log, Log.changeset_for_insert(artist))
  |> Multi.run(:search, SearchEngine, :update, ["extra argument"])

assert %Multi{} = multi

multi =
  Multi.new()
  |> Multi.insert(:artist, artist)
  |> Multi.insert(:log, Log.changeset_for_insert(artist))
  |> Multi.run(:search, SearchEngine, :update, ["extra argument"])
Multi.to_list(multi)
#=> [
#=>   artist: {:insert,
#=>    #Ecto.Changeset<action: :insert, changes: %{}, errors: [],
#=>     data: #MusicDB.Artist<>, valid?: true>, []},
#=>   log: {:insert,
#=>    #Ecto.Changeset<action: :insert, changes: %{}, errors: [],
#=>     data: #MusicDB.Log<>, valid?: true>, []},
#=>   search: {:run, {SearchEngine, :update, ["extra argument"]}}
#=> ]

assert [artist: {:insert, %Changeset{}, []},
  log: {:insert, %Changeset{}, []},
  search: {:run, {SearchEngine, :update, ["extra argument"]}}] = Multi.to_list(multi)
