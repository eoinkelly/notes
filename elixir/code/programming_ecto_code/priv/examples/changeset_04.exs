#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
import ExUnit.Assertions

import Ecto.Changeset
alias MusicDB.Artist

params = %{"name" => "Thelonius Monk", "birth_date" => "1917-10-10"}
changeset =
  %Artist{}
  |> cast(params, [:name, :birth_date])
  |> validate_required([:name, :birth_date])
  |> validate_length(:name, min: 3)

assert changeset.valid?

params = %{"name" => "Thelonius Monk"}
changeset =
  %Artist{}
  |> cast(params, [:name, :birth_date])
  |> validate_required([:name, :birth_date])
  |> validate_length(:name, min: 3)

changeset.valid?
#=> false
changeset.errors
#=> [birth_date: {"can't be blank", [validation: :required]}]

refute changeset.valid?
assert [birth_date: {"can't be blank", [validation: :required]}] == changeset.errors

params = %{"name" => "x"}
changeset =
  %Artist{}
  |> cast(params, [:name, :birth_date])
  |> validate_required([:name, :birth_date])
  |> validate_length(:name, min: 3)

changeset.errors
#=> [name: {"should be at least %{count} character(s)",
#=> [count: 3, validation: :length, min: 3]},
#=> birth_date: {"can't be blank", [validation: :required]}]

assert MapSet.new([:name, :birth_date]) == MapSet.new(Keyword.keys(changeset.errors))

result =
traverse_errors(changeset, fn {msg, opts} ->
  Enum.reduce(opts, msg, fn {key, value}, acc ->
    String.replace(acc, "%{#{key}}", to_string(value))
  end)
end)
#=> %{birth_date: ["can't be blank"],
#=> name: ["should be at least 3 character(s)"]}

assert %{birth_date: _birth_date, name: _name} = result
