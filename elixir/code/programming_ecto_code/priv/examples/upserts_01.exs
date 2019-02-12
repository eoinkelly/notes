#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
import ExUnit.Assertions

alias MusicDB.Repo

result =
Repo.insert_all("genres", [[name: "ska", wiki_tag: "Ska_music"]])
# => {1, nil}

assert {1, nil} == result

if Repo.using_postgres?() do
  assert_raise(Postgrex.Error, fn ->
    Repo.insert_all("genres", [[name: "ska", wiki_tag: "Ska_music"]])
    # => ** (Postgrex.Error) ERROR 23505 (unique_violation): duplicate key
    # => value violates unique constraint "genres_name_index"
  end)
else
  assert_raise(Mariaex.Error, fn ->
    Repo.insert_all("genres", [[name: "ska", wiki_tag: "Ska_music"]])
  end)
end

result =
Repo.insert_all("genres", [[name: "ska", wiki_tag: "Ska_music"]],
  on_conflict: :nothing)
# => {0, nil}

if Repo.using_postgres?() do
  assert {0, nil} == result
  # MySQL returns {1, nil}
end

assert_raise(ArgumentError, fn ->
  Repo.insert_all("genres", [[name: "ska", wiki_tag: "Ska"]],
    on_conflict: {:replace, [:wiki_tag]}, returning: [:wiki_tag])
  #=> ** (ArgumentError) :conflict_target option is required
  #=> when :on_conflict is replace
end)

# MySQL doesn't support the :returning option
if Repo.using_postgres?() do
  result =
  Repo.insert_all("genres", [[name: "ska", wiki_tag: "Ska"]],
    on_conflict: {:replace, [:wiki_tag]}, conflict_target: :name,
    returning: [:wiki_tag])
  # => {1, [%{wiki_tag: "Ska"}]}

  assert {1, [%{wiki_tag: "Ska"}]} == result

  result =
  Repo.insert_all("genres", [[name: "ambient", wiki_tag: "Ambient_music"]],
    on_conflict: {:replace, [:wiki_tag]}, conflict_target: :name,
    returning: [:wiki_tag])
  # => {1, [%{wiki_tag: "Ambient_music"}]}

  assert {1, [%{wiki_tag: "Ambient_music"}]} == result

  Repo.insert_all("genres", [[name: "ambient", wiki_tag: "Ambient_music"]],
    on_conflict: [set: [wiki_tag: "Ambient_music"]],
    conflict_target: :name, returning: [:wiki_tag])

  assert {1, [%{wiki_tag: "Ambient_music"}]} == result
end
