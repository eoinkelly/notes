#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
defmodule MusicDB.DateTimeUnix do
  @behaviour Ecto.Type

  def type(), do: :datetime

end

defmodule MusicDB.DateTimeUnix do
  @behaviour Ecto.Type

  def type(), do: :datetime

  def dump(term), do: Ecto.Type.dump(:datetime, term)

  def load(term), do: Ecto.Type.load(:datetime, term)
end

defmodule MusicDB.DateTimeUnix do
  @behaviour Ecto.Type

  def type(), do: :datetime

  def dump(term), do: Ecto.Type.dump(:datetime, term)

  def load(term), do: Ecto.Type.load(:datetime, term)

  def cast("Date(" <> rest) do
    with {unix, ")"} <- Integer.parse(rest),
         {:ok, datetime} <- DateTime.from_unix(unix)
    do
      {:ok, datetime}
    else
      _ -> :error
    end
  end
  def cast(%DateTime{} = datetime), do: {:ok, datetime}
  def cast(_other), do: :error
end

defmodule MusicDB.Album do
  use Ecto.Schema

  schema "albums" do
    field :last_viewed, MusicDB.DateTimeUnix
    #...
  end

end

defmodule EctoVersion do
  @behaviour Ecto.Type

end

defmodule EctoVersion do
  @behaviour Ecto.Type

  def type(), do: :string

end

defmodule EctoVersion do
  @behaviour Ecto.Type

  def type(), do: :string

  def dump(%Version{} = version), do: {:ok, to_string(version)}

  def load(string), do: Version.parse(string)

end

defmodule EctoVersion do
  @behaviour Ecto.Type

  def type(), do: :string

  def dump(%Version{} = version), do: {:ok, to_string(version)}
  def dump(_), do: :error

  def load(string) when is_binary(string), do: Version.parse(string)
  def load(_), do: :error

  def cast(string) when is_binary(string), do: Version.parse(string)
  def cast(_other), do: :error
end


