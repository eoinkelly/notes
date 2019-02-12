#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
import ExUnit.Assertions

alias MusicDB.{Repo, Artist, Log}

artist = %Artist{name: "Johnny Hodges"}
Repo.insert(artist)
Repo.insert(Log.changeset_for_insert(artist))

result = (fn ->
  artist = %Artist{name: "Johnny Hodges"}
  Repo.transaction(fn ->
    Repo.insert!(artist)
    Repo.insert!(Log.changeset_for_insert(artist))
  end)
  #=> {:ok, %MusicDB.Log{ ...}}
end).()

assert {:ok, %Log{}} = result

assert_raise(FunctionClauseError, fn ->
  artist = %Artist{name: "Ben Webster"}
  Repo.transaction(fn ->
    Repo.insert!(artist)
    Repo.insert!(nil) # <-- this will fail
  end)
  #=> ** (FunctionClauseError) no function clause matching in
  #=> Ecto.Repo.Schema.insert/4
end)

Repo.get_by(Artist, name: "Ben Webster")
# => nil

assert nil == Repo.get_by(Artist, name: "Ben Webster")
