#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---

defmodule MusicDB.Track do
  defstruct [:id, :title, :duration, :index, :number_of_plays]
end

defmodule MusicDB.Track do
  use Ecto.Schema

  schema "tracks" do
    field :title, :string
    field :duration, :integer
    field :index, :integer
    field :number_of_plays, :integer
    timestamps()
  end

end

defmodule SchemaExample do
  use Ecto.Schema
  schema "examples" do
    field :track_id, :id, primary_key: true
  end
end
