#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
alias MusicDB.Repo

defmodule MusicDB.Repo.Migrations.AddIndexesToCompositions do
  use Ecto.Migration

  def change do
    create index("compositions", :title)
    create index("compositions", :year)
  end
end

Ecto.Migrator.up(Repo, 1, MusicDB.Repo.Migrations.AddIndexesToCompositions)

defmodule MusicDB.Repo.Migrations.MiscIndexes do
  use Ecto.Migration

  def change do
    # create an index on the title and year columns together
    create index("compositions", [:title, :year])

    create index("genres", :name, unique: true)

    create unique_index("genres", :name)

    create index("compositions", :title, name: "title_index")
  end
end
