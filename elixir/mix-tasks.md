# Built-ins

* Erlang is more than a language, it is also an operating system for your applications.
* Erlang developers rarely write standalone modules, they write libraries or applications, and then bundle those into what is called a release.
* A release contains the Erlang VM plus all applications required to run the node, so it can be pushed to production directly.

* `mix` `elixirc`, `elixir`, `iex` commands come with elixir and live in the exlixir install dir
* `/Users/eoinkelly/.exenv/versions/1.1.1/lib/mix/ebin
* mix is a mix.app and a bunch of .beam files

# Archives

* Erlang archives are ZIP files with .ez extension.
* Erlang archives may also be enclosed in escript files whose file extension is arbitrary
* Erlang archive files may contain entire Erlang applications or parts of applications.
* The structure in an archive file is the same as the directory structure for an application.
* Archives are kept in `~/.mix/archives`
* Currently my installation only contains `.ez` files for `phoenix_new` and `hex`

How does mix find tasks and files and know what to build?

naming convention?


# Standard mix tasks

## mix deps

* only makes sense within an existing project dir
* manages the dependencies of a particular project - a bit like `bundler`
* `mix hex.outdated` shows only the out of date depencencies
* `mix deps` shows all dependences

## mix hex

* interacts with the package manager
* `mix hex ...` is a bit like `gem ...` in ruby

## mix archive

* lists the archives installed on the machine - not specific to a particular project
* archives are a way of distributing erlang code, hex is more recent and comes from elixir land
* you install an archive by `mix archive.install https://github.com/path/to/someproject-1.0.3.ez`

## mix local

* ???

```
iex -S mix              # Starts IEx and run the default task
mix                     # Runs the default task (current: "mix run")
mix app.start           # Starts all registered apps
mix archive             # Lists all archives
mix archive.build       # Archives this project into a .ez file
mix archive.install     # Installs an archive locally
mix archive.uninstall   # Uninstalls archives
mix clean               # Deletes generated application files
mix cmd                 # Executes the given command
mix compile             # Compiles source files
mix deps                # Lists dependencies and their status
mix deps.clean          # Deletes the given dependencies' files
mix deps.compile        # Compiles dependencies
mix deps.get            # Gets all out of date dependencies
mix deps.unlock         # Unlocks the given dependencies
mix deps.update         # Updates the given dependencies
mix do                  # Executes the tasks separated by comma
mix escript.build       # Builds an escript for the project
mix help                # Prints help information for tasks
mix hex                 # Prints Hex help information
mix hex.build           # Builds a new package version locally
mix hex.config          # Reads or updates Hex config
mix hex.docs            # Publishes docs for package
mix hex.info            # Prints Hex information
mix hex.key             # Hex API key tasks
mix hex.outdated        # Shows outdated Hex deps for the current project
mix hex.owner           # Hex package ownership tasks
mix hex.publish         # Publishes a new package version
mix hex.registry        # Hex registry tasks
mix hex.search          # Searches for package names
mix hex.user            # Hex user tasks
mix loadconfig          # Loads and persists the given configuration
mix local               # Lists local tasks
mix local.hex           # Installs Hex locally
mix local.public_keys   # Manages public keys
mix local.rebar         # Installs rebar locally
mix new                 # Creates a new Elixir project
mix phoenix.new         # Create a new Phoenix v0.16.1 application
mix profile.fprof       # Profiles the given file or expression with fprof
mix run                 # Runs the given file or expression
mix test                # Runs a project's tests
```

# Extra mix tasks added by Phoenix

```
mix conform.configure   # Create a .conf file from schema and project config.
mix conform.effective   # Print the effective configuration for the current project
mix conform.new         # Create a new .schema.exs file for configuring your app with conform.
mix ecto.create         # Create the storage for the repo
mix ecto.drop           # Drop the storage for the repo
mix ecto.gen.migration  # Generate a new migration for the repo
mix ecto.gen.repo       # Generate a new repository
mix ecto.migrate        # Run migrations up on a repo
mix ecto.rollback       # Rollback migrations from a repo
mix phoenix.digest      # Digests and compress static files
mix phoenix.gen.channel # Generates a Phoenix channel
mix phoenix.gen.html    # Generates controller, model and views for an HTML based resource
mix phoenix.gen.json    # Generates a controller and model for a JSON based resource
mix phoenix.gen.model   # Generates an Ecto model
mix phoenix.gen.secret  # Generates a secret
mix phoenix.routes      # Prints all routes
mix phoenix.server      # Starts applications and their servers
mix release             # Build a release for the current mix application.
mix release.clean       # Clean up any release-related files.
mix release.plugins     # View information about active release plugins
```
