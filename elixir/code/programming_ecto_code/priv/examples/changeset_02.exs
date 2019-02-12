#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
import ExUnit.Assertions

import Ecto.Changeset
import Ecto.Query
alias Ecto.Changeset
alias MusicDB.{Repo, Artist}

import Ecto.Changeset

changeset = change(%Artist{name: "Charlie Parker"})

assert %Changeset{} = changeset

artist = Repo.one(from a in Artist, where: a.name == "Bobby Hutcherson")
changeset = change(artist)

assert %Changeset{} = changeset

artist = Repo.one(from a in Artist, where: a.name == "Bobby Hutcherson")
changeset = change(artist, name: "Robert Hutcherson")

assert %Changeset{} = changeset

changeset.changes
#=> %{name: "Robert Hutcherson"}

assert %{name: "Robert Hutcherson"} = changeset.changes

changeset = change(changeset, birth_date: ~D[1941-01-27])

assert %{name: "Robert Hutcherson", birth_date: ~D[1941-01-27]} = changeset.changes

artist = Repo.one(from a in Artist, where: a.name == "Bobby Hutcherson")
changeset = change(artist, name: "Robert Hutcherson",
  birth_date: ~D[1941-01-27])

assert %Changeset{} = changeset

changeset.changes
#=> %{birth_date: ~D[1941-01-27], name: "Robert Hutcherson"}

assert %{name: "Robert Hutcherson", birth_date: ~D[1941-01-27]} = changeset.changes

