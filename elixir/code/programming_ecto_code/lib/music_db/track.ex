#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
defmodule MusicDB.Track do
  use Ecto.Schema
  alias MusicDB.Album

  schema "tracks" do
    field(:title, :string)
    field(:duration, :integer)
    field(:duration_string, :string, virtual: true)
    field(:index, :integer)
    field(:number_of_plays, :integer)
    timestamps()

    belongs_to(:album, Album)
  end
end
