#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
ExUnit.start()

defmodule MusicDB.AlbumTest do
  use ExUnit.Case, async: true

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(MusicDB.Repo)
  end

  test "insert album" do
    task = Task.async(fn ->
      album = MusicDB.Repo.insert!(%MusicDB.Album{title: "Giant Steps"})
      album.id
    end)

    album_id = Task.await(task)
    assert MusicDB.Repo.get(MusicDB.Album, album_id).title == "Giant Steps"
  end
end

defmodule MusicDB.AlbumTest2 do
  use ExUnit.Case

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(MusicDB.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(MusicDB.Repo, {:shared, self()})
  end

  test "dummy test" do
    assert true
  end
end

