#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
defmodule MusicDB.Repo.Migrations.AddNotesJoinTables do
  use Ecto.Migration

  def change do
    create table(:artists_notes) do
      add :artist_id, references(:artists)
      add :note_id, references(:notes_with_joins)
    end
    create index(:artists_notes, :artist_id)
    create index(:artists_notes, :note_id)

    create table(:albums_notes) do
      add :album_id, references(:albums)
      add :note_id, references(:notes_with_joins)
    end
    create index(:albums_notes, :album_id)
    create index(:albums_notes, :note_id)

    create table(:tracks_notes) do
      add :track_id, references(:tracks)
      add :note_id, references(:notes_with_joins)
    end
    create index(:tracks_notes, :track_id)
    create index(:tracks_notes, :note_id)

  end
end
