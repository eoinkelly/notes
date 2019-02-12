#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
defmodule MusicDB.AlbumTest do
  use ExUnit.Case, async: true
  alias MusicDB.{Repo, Album}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(MusicDB.Repo)
  end

  test "insert album" do
    album = MusicDB.Repo.insert!(%MusicDB.Album{title: "Giant Steps"})
    new_album = MusicDB.Repo.get!(MusicDB.Album, album.id)
    assert new_album.title == "Giant Steps"
  end

  test "valid changeset" do
    params = %{"title" => "Dark Side of the Moon"}
    changeset = Album.changeset(%Album{}, params)
    album = Repo.insert!(changeset)
    assert album.title == "Dark Side of the Moon"
  end

  test "valid changeset without insert" do
    params = %{"title" => "Dark Side of the Moon"}
    changeset = Album.changeset(%Album{}, params)
    album = Ecto.Changeset.apply_changes(changeset)
    assert album.title == "Dark Side of the Moon"
  end

end
