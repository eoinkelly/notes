#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
defmodule MusicDB.Repo.Migrations.AddAlbumsWithEmbeds do
  use Ecto.Migration

  def change do
    create table("albums_with_embeds") do
      add(:title, :string)
      if MusicDB.Repo.using_postgres?() do
        add(:artist, :jsonb)
        add(:tracks, {:array, :jsonb}, default: [])
      end
    end
  end
end
