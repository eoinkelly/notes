#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
defmodule MusicDB.Repo.Migrations.AddArtistsTable do
  use Ecto.Migration

  def change do
    create table(:artists) do
      add :name, :string, null: false
      add :birth_date, :date, null: true
      add :death_date, :date, null: true

      # normally you would not make this nullable - we did this here
      # just to simplify some examples
      timestamps null: true
    end

    create index(:artists, :name)
  end
end
