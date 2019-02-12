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
alias Ecto.Query

q = from "artists", where: [name: "Bill Evans"], select: [:id, :name]

assert %Query{} = q

q = from a in "artists", where: a.name == "Bill Evans", select: [:id, :name]

assert %Query{} = q
