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

result = (fn ->
  artist = %Artist{name: "Johnny Hodges"}
  Repo.transaction(fn ->
    artist_record = Repo.insert!(artist)
    Repo.insert!(Log.changeset_for_insert(artist_record))
    SearchEngine.update!(artist_record)
  end)
end).()

assert {:ok, _result} = result

