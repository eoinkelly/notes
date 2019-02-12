#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
defmodule MyApp.MixProject do
  defp deps do
    [
      {:postgrex, ">= 0.0.0"},
      {:ecto_sql, "~> 3.0"}
    ]
  end
end

_ = """
defmodule MyApp.Repo do
  use Ecto.Repo,
    otp_app: :my_app,
    adapter: Ecto.Adapters.Postgres
end
"""

_ = """
config :my_app, MyApp.Repo,
  database: "my_database",
  username: "postgres",
  password: "postgres",
  hostname: "localhost"
"""

_ = """
config :my_app, :ecto_repos, [MyApp.Repo]
"""

# List all child processes to be supervised
children = [
  MyApp.Repo
]

# for Elixir 1.4
import Supervisor.Spec, warn: false

children = [
  supervisor(MyApp.Repo, [])
]

_ = """
defmodule MyApp.OtherRepo do
  use Ecto.Repo, otp_app: :my_app, adapter: Ecto.Adapter.Postgres
end
"""

_ = """
config :my_app, MyApp.OtherRepo, ...

config :my_app, :ecto_repos, [MyApp.Repo, MyApp.OtherRepo]
"""

children = [
  MyApp.Repo,
  MyApp.OtherRepo
]

_ = """
MyApp.Repo.aggregate("some_table", :count, :some_column)
"""

