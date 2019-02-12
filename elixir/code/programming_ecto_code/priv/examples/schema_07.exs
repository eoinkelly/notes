#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
defmodule BelongsToAssociations do
  use Ecto.Schema

  schema "tracks" do
    field :title, :string
    # other fields here...

    belongs_to :album, MusicDB.Album
  end
end

defmodule BelongsToAssociations2 do
  use Ecto.Schema

  # in album.ex
  schema "albums" do
    # field definitions here...

    has_many :tracks, MusicDB.Track
    belongs_to :artist, MusicDB.Artist
  end
end

defmodule BelongsToAssociations3 do
  use Ecto.Schema

  # in artist.ex
  schema "artists" do
    # field definitions here...

    has_many :albums, MusicDB.Album
  end

end
