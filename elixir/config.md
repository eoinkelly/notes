# Configuring elixir


There are 2 times you need configuration for your code

1. compile time
1. runtime


There are ? methods for providing configuration to elixir code

# 1. Mix config
* runs at both compile time and runtime?

The following scripts are evaluated **every** time you run mix:

* `mix.exs`
* `config/config.exs` and anything it includes e.g. `config/dev.exs`

including `mix compile`. The flip-side of this is that these files are not evaluated if you don't start your app with mix! In a relase we don't use mix so what happens is the config gets baked into an erlang `sys.config` at **build time**.

This means that any usage of `System.get_env/1` in our mix config doesn't pull from the server's env as we would expect, it pulls from the build machine's env.


# 2. Erlang `sys.config` file

This is where releases expect to find their config

# Distillery

We need a way to load configuration at _runtime_ e.g. so we can pull configuration values from the ENV.

Distillery solves that by building the final `sys.config` at runtime

## The applicaiton boot process

Just **before** application boot:

1. Take the `sys.config` which is baked into the release
1. Run each configured "config provider" module in turn and layer its changes on top of the built-in `sys.config`

Once a final `sys.config` has been built, the release will start as normal

The distillery configuration is controlled by `rel/config.exs`

It has "config provider" modules which know how to pull config from

1. Toml
1. Yaml
1. JSON
1. Etcd
1. Consul
1. Mix.Config
    * you have to opt-in to this
    * you should use a separate config file e.g. `rel/config/config.exs` - don't just re-use your normal mix config because that config might make assumtions about things like Mix.Project being available
