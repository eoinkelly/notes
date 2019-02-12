#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

# This configuration is loaded before any dependency and is restricted
# to this project. If another project depends on this project, this
# file won't be loaded nor affect the parent project. For this reason,
# if you want to provide default values for your application for
# 3rd-party users, it should be done in your "mix.exs" file.

# You can configure your application as:
#
#     config :music_db, key: :value
#
# and access this configuration in your application as:
#
#     Application.get_env(:music_db, :key)
#
# You can also configure a 3rd-party app:
#
#     config :logger, level: :info
#

config :music_db, :ecto_repos, [MusicDB.Repo]

config :music_db, MusicDB.Repo,
  # username: your_username,
  # password: your_password,
  database: "music_db",
  hostname: "localhost",
  migration_lock: nil # this is not normally needed - we put it here to support an example of
                      # creating an index with the `concurrently` option set to true

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
