#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
defmodule MusicDB.Genre do
  use Ecto.Schema
  alias MusicDB.Album

  schema "genres" do
    field(:name)
    field(:wiki_tag)
    timestamps()

    many_to_many(:albums, Album, join_through: "albums_genres")
  end
end
