#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
alias MusicDB.{Repo, Album, Note}

defmodule MusicDB.Album do
  use Ecto.Schema

  schema "albums" do
    field(:title, :string)
    many_to_many :notes, MusicDB.Note, join_through: "albums_notes"
  end
end

defmodule MusicDB.Note do
  use Ecto.Schema

  schema "notes_with_joins" do
    field :note, :string
    field :author, :string
    many_to_many :albums, MusicDB.Album, join_through: "albums_notes"
    timestamps()
  end
end

result = (fn ->
  album = Repo.get_by(Album, title: "Kind Of Blue")
  note = Repo.insert!(%Note{note: "Love this album!", author: "darin"})
  album
  |> Repo.preload(:notes)
  |> Ecto.Changeset.change()
  |> Ecto.Changeset.put_assoc(:notes, [note])
  |> Repo.update!
  album = Repo.preload(album, :notes)
  album.notes
end)

