#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
import ExUnit.Assertions

alias MusicDB.{Repo, Artist, Album, Track, Genre}

result =
Repo.insert_all("artists", [[name: "John Coltrane"]])
#=> {1, nil}

assert {1, nil} = result

if Repo.using_postgres?() do
  result =
  Repo.insert_all("artists", [[name: "John Coltrane"]], returning: [:id])
  #=> {1, [%{id: 8}]}

  assert {1, [%{id: _id}]} = result
end


result =
Repo.insert(%Artist{name: "John Coltrane"})

assert {:ok, %Artist{}} = result

{:ok, artist} = Repo.insert(%Artist{name: "John Coltrane"})
#=> %MusicDB.Artist{__meta__: #Ecto.Schema.Metadata<:loaded, "artists">,
#=> albums: #Ecto.Association.NotLoaded<association :albums is not loaded>,
#=> id: 8, inserted_at: ~N[2017-07-14 06:35:05],
#=> name: "John Coltrane",
#=> tracks: #Ecto.Association.NotLoaded<association :tracks is not loaded>,
#=> updated_at: ~N[2017-07-14 06:35:05]}

result =
Repo.insert(
  %Artist{
    name: "John Coltrane",
    albums: [
      %Album{
        title: "A Love Supreme"
      }
    ]
  }
)

assert {:ok, %Artist{albums: [%Album{}]}} = result

result =
Repo.insert(
  %Artist{
    name: "John Coltrane",
    albums: [
      %Album{
        title: "A Love Supreme",
        tracks: [
          %Track{title: "Part 1: Acknowledgement", index: 1},
          %Track{title: "Part 2: Resolution", index: 2},
          %Track{title: "Part 3: Pursuance", index: 3},
          %Track{title: "Part 4: Psalm", index: 4},
        ],
        genres: [
          %Genre{name: "spiritual jazz"},
        ]
      }
    ]
  }
)

assert {:ok, %Artist{
  albums: [%Album{
    tracks: [%Track{}, %Track{}, %Track{}, %Track{}],
    genres: [%Genre{}]
  }]}} = result
