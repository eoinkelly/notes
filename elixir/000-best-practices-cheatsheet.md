```
# change iex prompt to you can copy paste to doctests without faffing with the prompt
IEx.configure(default_prompt: "%prefix>")
```

```
erl # show erlang version by entering the shell (actually seems to be the easiest way)

exenv versions # what versions do I have
exenv install 1.7.3
exenv global 1.7.3

mix local.hex # install or upgrade hex package manager
mix hex # show hex version and details

mix local.phx # upgrade phoenix

mix archive # show installed archives (will contain at least phx and hex)

QUESTIOQuestion: waht is the deail with mix local.phoenix


mix phx.new my_app_name # generate a new phoenix app


# you can read back your application configuration in whole or by keys
iex> Application.get_all_env(:name_of_otp_app)
iex> Application.get_env(:name_of_otp_app, NameOfModuleKey)
iex> Application.get_env(:name_of_otp_app, Repo)

# List the active applications
TODO

# Get the full environment (the erlang one, not the unix one) for a given application
iex> Application.get_all_env(:ackama_hub)

# Set MIX_DEBUG to see more about what is going on
MIX_DEBUG=1 mix compile

mix copies the "application environment" into the APP_NAME.app resource file along with its .beam files
this resource file is read when the app is loaded

## Introspecting modules ##
###########################

# 1. see list of functions and arities
iex> MyModule.__info__(:functions)
# 2. see docs
iex> h MyModule
# 3. see type information
iex> i MyModule






[
  {:ecto_repos, [AckamaHub.Repo]},
  {AckamaHubWeb.Endpoint,
   [
     url: [host: "localhost"],
     secret_key_base: "V7G0T2+2APCUq/ifG6KTzCNtmvlahXVaPHKDe3+pai9znaAoEKmS99eAQyvDM4nC",
     render_errors: [view: AckamaHubWeb.ErrorView, accepts: ["html", "json"]],
     pubsub: [name: AckamaHub.PubSub, adapter: Phoenix.PubSub.PG2],
     http: [port: 4000],
     debug_errors: true,
     code_reloader: true,
     check_origin: false,
     watchers: [
       node: [
         "node_modules/brunch/bin/brunch",
         "watch",
         "--stdin",
         {:cd,
          "/Users/eoinkelly/Code/bitbucket.org/rabidtech/ackama_hub/assets"}
       ]
     ],
     live_reload: [
       patterns: [~r/priv\/static\/.*(js|css|png|jpeg|jpg|gif|svg)$/,
        ~r/priv\/gettext\/.*(po)$/, ~r/lib\/ackama_hub_web\/views\/.*(ex)$/,
        ~r/lib\/ackama_hub_web\/templates\/.*(eex)$/]
     ]
   ]},
  {AckamaHub.Repo,
   [
     adapter: Ecto.Adapters.Postgres,
     username: "postgres",
     password: "postgres",
     database: "ackama_hub_dev",
     hostname: "localhost",
     pool_size: 10
   ]}
]
```
