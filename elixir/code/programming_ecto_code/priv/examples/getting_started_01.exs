#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
alias MusicDB.{Repo, Artist}

# insert a record into the artists table
Repo.insert(%Artist{name: "Dizzy Gillespie"})

# retrieve the record
dizzy = Repo.get_by(Artist, name: "Dizzy Gillespie")

# make a change
Repo.update(Ecto.Changeset.change(dizzy, name: "John Birks Gillespie"))

# retrieve it again
dizzy = Repo.get_by(Artist, name: "John Birks Gillespie")

# delete it
Repo.delete(dizzy)

