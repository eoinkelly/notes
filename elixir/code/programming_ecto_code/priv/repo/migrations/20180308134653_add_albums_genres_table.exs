#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
defmodule MusicDB.Repo.Migrations.AddAlbumsGenresTable do
  use Ecto.Migration

  def change do
    create table(:albums_genres) do
      add :album_id, references(:albums)
      add :genre_id, references(:genres)
    end

    create index(:albums_genres, :album_id)
    create index(:albums_genres, :genre_id)
  end
end
