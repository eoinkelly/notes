#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
import ExUnit.Assertions

alias MusicDB.{Repo, AlbumWithEmbeds, TrackEmbed}

Repo.get_by(AlbumWithEmbeds, title: "Moanin'")
#=> %MusicDB.AlbumWithEmbeds{
#=>  __meta__: #Ecto.Schema.Metadata<:loaded, "albums_with_embeds">,
#=>  artist: %MusicDB.ArtistEmbed{
#=>    id: "cab33f94-ecfb-461e-83a8-5ace0e02b9ca",
#=>    name: "Art Blakey"
#=>  },
#=>  id: 1,
#=>  title: "Moanin'",
#=>  tracks: [
#=>    %MusicDB.TrackEmbed{
#=>      duration: 575,
#=>      id: "7a8ae464-68fc-4320-a1a1-f555b3be74ba",
#=>      title: "Moanin'"
#=>    },
#=>    %MusicDB.TrackEmbed{
#=>      duration: 290,
#=>      id: "551a4623-a1eb-4bbc-9d30-024e3fce10e2",
#=>      title: "Are You Real"
#=>    },
#=>    ...

assert %AlbumWithEmbeds{title: "Moanin'"} = Repo.get_by(AlbumWithEmbeds, title: "Moanin'")

