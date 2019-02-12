#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
defmodule MusicDB.Note do
  use Ecto.Schema
  import Ecto.Changeset

  schema "notes_with_fk_fields" do
    field(:note, :string)
    field(:author, :string)
    timestamps()
    belongs_to(:artist, MusicDB.Artist)
    belongs_to(:album, MusicDB.Album)
    belongs_to(:track, MusicDB.Track)
  end

  _ = """
  schema "abstract table: notes" do
    field :note, :string
    field :author, :string
    field :assoc_id, :integer
    timestamps()
  end
  """

  _ = """
  schema "notes_with_joins" do
    field :note, :string
    field :author, :string
    many_to_many :artists, MusicDB.Artist, join_through: "artists_notes"
    many_to_many :albums, MusicDB.Album, join_through: "albums_notes"
    many_to_many :tracks, MusicDB.Track, join_through: "tracks_notes"
    timestamps()
  end
  """

  def changeset(note, params) do
    note
    |> cast(params, [:note, :author, :artist_id, :album_id, :track_id])
    |> validate_required([:note, :author])
    |> check_constraint(:artist_id,
      name: "only_one_fk",
      message: "You must set only one foreign key: artist_id, album_id, or track_id"
    )
  end
end
