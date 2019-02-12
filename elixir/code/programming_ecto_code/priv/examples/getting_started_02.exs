#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
import ExUnit.Assertions

alias MusicDB.Repo

result =
Repo.insert_all("artists", [[name: "John Coltrane"]])
#=> {1, nil}

assert {1, nil} = result

result =
Repo.insert_all("artists",
  [[name: "Sonny Rollins", inserted_at: DateTime.utc_now()]])
#=> {1, nil}

assert {1, nil} = result

result =
Repo.insert_all("artists",
  [[name: "Max Roach", inserted_at: DateTime.utc_now()],
  [name: "Art Blakey", inserted_at: DateTime.utc_now()]])
#=> {2, nil}

assert {2, nil} = result

result =
Repo.insert_all("artists",
  [%{name: "Max Roach", inserted_at: DateTime.utc_now()},
   %{name: "Art Blakey", inserted_at: DateTime.utc_now()}])
#=> {2, nil}

assert {2, nil} = result

result =
Repo.update_all("artists", set: [updated_at: DateTime.utc_now()])
#=> {9, nil}

assert {9, nil} = result

result =
Repo.delete_all("tracks")
#=> {33, nil}

assert {33, nil} = result
