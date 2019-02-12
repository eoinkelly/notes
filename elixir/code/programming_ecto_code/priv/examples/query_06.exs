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

# like statements
q = from a in "artists", where: like(a.name, "Miles%"), select: [:id, :name]


assert %Query{} = q

# checking for null
q = from a in "artists", where: is_nil(a.name), select: [:id, :name]


assert %Query{} = q

# checking for not null
q = from a in "artists", where: not is_nil(a.name), select: [:id, :name]


assert %Query{} = q

# date comparison - this finds artists added more than 1 year ago
q = from a in "artists", where: a.inserted_at < ago(1, "year"),
  select: [:id, :name]

assert %Query{} = q

