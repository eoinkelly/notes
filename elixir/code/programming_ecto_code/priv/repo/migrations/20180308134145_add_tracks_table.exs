#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
defmodule MusicDB.Repo.Migrations.AddTracksTable do
  use Ecto.Migration

  def change do
    create table(:tracks) do
      add :title, :string, null: false
      add :duration, :integer, null: true
      add :index, :integer, null: false
      add :number_of_plays, :integer, null: false, default: 0
      add :album_id, references(:albums, on_delete: :nothing)
      timestamps()
    end

    create index(:tracks, :title)
    create index(:tracks, :album_id)
  end
end
