#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
defmodule NestedAssociation do
  use Ecto.Schema

  schema "artists" do
    # field definitions here...

    has_many :albums, MusicDB.Album
    has_many :tracks, through: [:albums, :tracks]
  end
end

