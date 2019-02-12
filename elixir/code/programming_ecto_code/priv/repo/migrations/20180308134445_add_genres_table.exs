#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
defmodule MusicDB.Repo.Migrations.AddGenresTable do
  use Ecto.Migration

  def change do
    create table(:genres) do
      add :name, :string, null: false
      add :wiki_tag, :string, null: true
      timestamps null: true
    end
    create index(:genres, :name, unique: true)
  end
end
