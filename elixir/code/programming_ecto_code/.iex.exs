#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
alias MusicDB.{
  Repo,
  Artist,
  Album,
  Track,
  Genre,
  Log,
  AlbumWithEmbeds,
  ArtistEmbed,
  TrackEmbed,
  Note
}

import_if_available Ecto.Query

import_if_available Ecto.Changeset
