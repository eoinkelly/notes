#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
import ExUnit.Assertions
import ExUnit.CaptureIO

alias MusicDB.{Repo, Artist, Log}

result = capture_io(fn ->
  cs = Ecto.Changeset.change(%Artist{name: nil})
    |> Ecto.Changeset.validate_required([:name])
  Repo.transaction(fn ->
    case Repo.insert(cs) do
      {:ok, _artist} -> IO.puts("Artist insert succeeded")
      {:error, _value} -> Repo.rollback("Artist insert failed")
    end
    case Repo.insert(Log.changeset_for_insert(cs)) do
      {:ok, _log} -> IO.puts("Log insert succeeded")
      {:error, _value} -> Repo.rollback("Log insert failed")
    end
  end)
end)

assert "" == result
