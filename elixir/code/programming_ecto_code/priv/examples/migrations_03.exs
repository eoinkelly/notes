#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
import ExUnit.Assertions

import Ecto.Query
alias MusicDB.Repo

defmodule MusicDB.Repo.Migrations.AddCompositionArtistsTable do
  use Ecto.Migration

  def change do
    create table("compositions_artists") do
      add :composition_id, references("compositions"), null: false
      add :artist_id, references("artists"), null: false
      add :role, :string, null: false
    end

    create index("compositions_artists", :composition_id)
    create index("compositions_artists", :artist_id)
  end
end

defmodule MusicDB.Repo.Migrations.AddCompositionArtistsTable2 do
  use Ecto.Migration
  import Ecto.Query

  def change do
    #...

    from(c in "compositions", select: [:id, :artist_id])
    |> Repo.all()
    |> Enum.each(fn row ->
      Repo.insert_all("compositions_artists", [
        [composition_id: row.id, artist_id: row.artist_id, role: "composer"]
      ])
    end)
  end
end

defmodule MusicDB.Repo.Migrations.AddCompositionArtistsTable3 do
  use Ecto.Migration
  def change do
    #...

    alter table("compositions") do
      remove :artist_id
    end
  end
end

defmodule MusicDB.Repo.Migrations.AddCompositionArtistsTable4 do
  use Ecto.Migration
  import Ecto.Query

  def change do
    #...

    create(index("compositions_artists", :composition_id))
    create(index("compositions_artists", :artist_id))

    flush()

    from(c in "compositions", select: [:id, :artist_id])
    |> Repo.all()

    #...

  end
end

defmodule MusicDB.Repo.Migrations.AddCompositionsArtistsTable do
  use Ecto.Migration
  import Ecto.Query
  alias MusicDB.Repo

  def change do
    create table("compositions_artists") do
      add(:composition_id, references("compositions"), null: false)
      add(:artist_id, references("artists"), null: false)
      add(:role, :string, null: false)
    end

    create(index("compositions_artists", :composition_id))
    create(index("compositions_artists", :artist_id))

    flush()

    from(c in "compositions", select: [:id, :artist_id])
    |> Repo.all()
    |> Enum.each(fn row ->
      Repo.insert_all("compositions_artists", [
        [composition_id: row.id, artist_id: row.artist_id, role: "composer"]
      ])
    end)

    alter table("compositions") do
      remove :artist_id
    end
  end
end

if Repo.using_postgres?() do
  Repo.insert_all("compositions", [[title: "So What", year: 1959, artist_id: 1,
    inserted_at: NaiveDateTime.utc_now(), updated_at: NaiveDateTime.utc_now()]])
  Ecto.Migrator.up(Repo, 2, MusicDB.Repo.Migrations.AddCompositionsArtistsTable)

  q = from c in "compositions", where: c.title == "So What", select: [:id, :title, :year]
  composition = Repo.one(q)
  assert %{title: "So What", year: 1959} = composition

  q = from ca in "compositions_artists", where: ca.composition_id == ^composition.id,
    select: [:composition_id, :artist_id, :role]
  composition_artist = Repo.one(q)
  assert %{artist_id: 1, role: "composer"} = composition_artist
end

