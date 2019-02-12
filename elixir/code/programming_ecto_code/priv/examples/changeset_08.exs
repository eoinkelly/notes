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

form = %{artist_name: :string, album_title: :string,
         artist_birth_date: :date, album_release_date: :date,
         genre: :string}

defmodule CustomValidations do
  import Ecto.Changeset

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
    form = %{artist_name: :string, album_title: :string,
              artist_birth_date: :date, album_release_date: :date,
              genre: :string}

    # user data - they only provided one value
    params = %{"artist_name" => "Ella Fitzgerald", "album_title" => "",
    "artist_birth_date" => "",  "album_release_date" => "",
    "genre" => ""}

    changeset =
      {%{}, form}
      |> cast(params, Map.keys(form))
      |> validate_in_the_past(:artist_birth_date)
      |> validate_in_the_past(:album_release_date)

    if changeset.valid? do
      # execute the advanced search
    else
      # show changeset.errors to the user
    end

    changeset.valid?
  end
end

assert CustomValidations.test_validation()

