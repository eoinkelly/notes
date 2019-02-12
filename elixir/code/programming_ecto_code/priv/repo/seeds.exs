#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
alias MusicDB.Repo
alias MusicDB.{Artist, Album, Track, Genre, AlbumWithEmbeds, ArtistEmbed, TrackEmbed}

jazz_genre = Repo.insert!(%Genre{ name: "jazz", wiki_tag: "Jazz" })
live_genre = Repo.insert!(%Genre{ name: "live", wiki_tag: "Concert" })

Repo.insert! %Artist{
  name: "Miles Davis",
  albums: [
    %Album{
      title: "Kind Of Blue",
      genres: [jazz_genre],
      tracks: [
        %Track{
          title: "So What",
          duration: 544,
          index: 1
        },
        %Track{
          title: "Freddie Freeloader",
          duration: 574,
          index: 2
        },
        %Track{
          title: "Blue In Green",
          duration: 327,
          index: 3
        },
        %Track{
          title: "All Blues",
          duration: 693,
          index: 4
        },
        %Track{
          title: "Flamenco Sketches",
          duration: 481,
          index: 5
        }
      ]
    },
    %Album{
      title: "Cookin' At The Plugged Nickel",
      genres: [jazz_genre, live_genre],
      tracks: [
        %Track{
          title: "If I Were A Bell",
          duration: 1006,
          index: 1
        },
        %Track{
          title: "Stella By Starlight",
          duration: 774,
          index: 2
        },
        %Track{
          title: "Walkin'",
          duration: 896,
          index: 3
        },
        %Track{
          title: "Miles",
          duration: 754,
          index: 4
        },
        %Track{
          title: "No Blues",
          duration: 1061,
          index: 5
        }
      ]
    }
  ]
}

Repo.insert! %Artist{
  name: "Bill Evans",
  albums: [
    %Album{
      title: "You Must Believe In Spring",
      genres: [jazz_genre],
      tracks: [
        %Track{
          title: "B Minor Waltz (for Ellaine)",
          duration: 192,
          index: 1
        },
        %Track{
          title: "You Must Believe In Spring",
          duration: 337,
          index: 2
        },
        %Track{
          title: "Gary's Theme",
          duration: 255,
          index: 3
        },
        %Track{
          title: "We Will Meet Again (for Harry)",
          duration: 239,
          index: 4
        },
        %Track{
          title: "The Peacocks",
          duration: 360,
          index: 5
        },
        %Track{
          title: "Sometime Ago",
          duration: 292,
          index: 6
        },
        %Track{
          title: "Theme From M*A*S*H (Suicide Is Painless)",
          duration: 353,
          index: 7
        },
        %Track{
          title: "Without a Song",
          duration: 485,
          index: 8
        },
        %Track{
          title: "Freddie Freeloader",
          duration: 454,
          index: 9
        },
        %Track{
          title: "All of You",
          duration: 489,
          index: 10
        }
      ]
    },
    %Album{
      title: "Portrait In Jazz",
      genres: [jazz_genre],
      tracks: [
        %Track{
          title: "Come Rain Or Come Shine",
          duration: 204,
          index: 1
        },
        %Track{
          title: "Autumn Leaves",
          duration: 360,
          index: 2
        },
        %Track{
          title: "Witchcraft",
          duration: 277,
          index: 3
        },
        %Track{
          title: "When I Fall In Love",
          duration: 297,
          index: 4
        },
        %Track{
          title: "Peri's Scope",
          duration: 195,
          index: 5
        },
        %Track{
          title: "What Is This Thing Called Love?",
          duration: 276,
          index: 6
        },
        %Track{
          title: "Spring Is Here",
          duration: 309,
          index: 7
        },
        %Track{
          title: "Someday My Prince Will Come",
          duration: 297,
          index: 8
        },
        %Track{
          title: "Blue In Green",
          duration: 325,
          index: 9
        }
      ]
    }
  ]
}

Repo.insert! %Artist{
  name: "Bobby Hutcherson",
  albums: [
    %Album{
      title: "Live At Montreaux",
      genres: [jazz_genre, live_genre],
      tracks: [
        %Track{
          title: "Anton's Ball",
          duration: 761,
          index: 1
        },
        %Track{
          title: "The Moontrane",
          duration: 647,
          index: 2
        },
        %Track{
          title: "Farallone",
          duration: 805,
          index: 3
        },
        %Track{
          title: "Song Of Songs",
          duration: 844,
          index: 4
        }
      ]
    }
  ]
}

if Repo.using_postgres?() do
  Repo.insert! %AlbumWithEmbeds{
    title: "Moanin'",
    artist: %ArtistEmbed{
      name: "Art Blakey"
    },
    tracks: [
      %TrackEmbed{
        title: "Moanin'",
        duration: 575
      },
      %TrackEmbed{
        title: "Are You Real",
        duration: 290
      },
      %TrackEmbed{
        title: "Along Came Betty",
        duration: 372
      },
      %TrackEmbed{
        title: "The Drum Thunder Suite",
        duration: 453
      },
      %TrackEmbed{
        title: "Blues March",
        duration: 377
      },
      %TrackEmbed{
        title: "Come Rain or Come Shine",
        duration: 349
      }
    ]
  }
end

IO.puts ""
IO.puts "Success! Sample data has been added."
