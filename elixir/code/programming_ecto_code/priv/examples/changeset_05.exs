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

params = %{"name" => "Thelonius Monk", "birth_date" => "2117-10-10"}
changeset =
  %Artist{}
  |> cast(params, [:name, :birth_date])
  |> validate_change(:birth_date, fn :birth_date, birth_date ->
    cond do
      is_nil(birth_date) -> []
      Date.compare(birth_date, Date.utc_today()) == :lt -> []
      true -> [birth_date: "must be in the past"]
    end
  end)
changeset.errors
#=> [birth_date: {"must be in the past", []}]

assert [birth_date: {"must be in the past", []}] == changeset.errors

defmodule CustomValidations do
  import Ecto.Changeset

  def validate_birth_date_in_the_past(changeset) do
    validate_change(changeset, :birth_date, fn :birth_date, birth_date ->
      cond do
        is_nil(birth_date) -> []
        Date.compare(birth_date, Date.utc_today()) == :lt -> []
        true -> [birth_date: "must be in the past"]
      end
    end)
  end

  def validate_in_the_past(changeset, field) do
    validate_change(changeset, field, fn _field, value ->
      cond do
        is_nil(value) -> []
        Date.compare(value, Date.utc_today()) == :lt -> []
        true -> [{field, "must be in the past"}]
      end
    end)
  end

  def test_validation do
    params = %{"name" => "Thelonius Monk", "birth_date" => "2117-10-10"}
    changeset =
      %Artist{}
      |> cast(params, [:name, :birth_date])
      |> validate_required(:name)
      |> validate_in_the_past(:birth_date)
  end

end

changeset = CustomValidations.test_validation()
assert [birth_date: {"must be in the past", []}] == changeset.errors
