#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
import ExUnit.Assertions

alias MusicDB.{Repo, Genre}

result = (fn ->
  genre = %Genre{name: "funk", wiki_tag: "Funk"}
  Repo.insert(genre)
  #=> {:ok,
  #=> %MusicDB.Genre{__meta__: #Ecto.Schema.Metadata<:loaded, "genres">,
  #=>  albums: #Ecto.Association.NotLoaded<association :albums is not loaded>,
  #=>  id: 3, inserted_at: ~N[2018-03-05 14:26:13], name: "funk",
  #=>  updated_at: ~N[2018-03-05 14:26:13], wiki_tag: "Funk"}}
end).()

assert {:ok, %Genre{name: "funk"}} = result

genre = %Genre{name: "funk", wiki_tag: "Funk"}

if Repo.using_postgres?() do
  result =
  Repo.insert(genre, on_conflict: [set: [wiki_tag: "Funk_music"]],
    conflict_target: :name)
  #=> {:ok,
  #=> %MusicDB.Genre{__meta__: #Ecto.Schema.Metadata<:loaded, "genres">,
  #=>  albums: #Ecto.Association.NotLoaded<association :albums is not loaded>,
  #=>  id: 3,inserted_at: ~N[2018-03-05 14:27:14], name: "funk",
  #=>  updated_at: ~N[2018-03-05 14:27:14], wiki_tag: "Funk"}}

  assert {:ok, %Genre{wiki_tag: "Funk"}} = result

  Repo.get(Genre, 3)
  #=> %MusicDB.Genre{__meta__: #Ecto.Schema.Metadata<:loaded, "genres">,
  #=>  albums: #Ecto.Association.NotLoaded<association :albums is not loaded>,
  #=>  id: 3,inserted_at: ~N[2018-03-05 14:26:13], name: "funk",
  #=>  updated_at: ~N[2018-03-05 14:26:13], wiki_tag: "Funk_music"}

  assert %Genre{name: "funk", wiki_tag: "Funk_music"} = Repo.get(Genre, 3)

  genre = %Genre{name: "funk", wiki_tag: "Funky_stuff"}
  Repo.insert(genre, on_conflict: :replace_all_except_primary_key,
    conflict_target: :name)
  #=> {:ok,
  #=>  %MusicDB.Genre{
  #=>    __meta__: #Ecto.Schema.Metadata<:loaded, "genres">,
  #=>    albums: #Ecto.Association.NotLoaded<association :albums is not loaded>,
  #=>    id: 3, inserted_at: ~N[2018-03-05 23:01:28], name: "funk",
  #=>    updated_at: ~N[2018-03-05 23:01:28], wiki_tag: "Funky_stuff" }}

  assert %Genre{name: "funk", wiki_tag: "Funky_stuff"} = Repo.get(Genre, 3)
end
