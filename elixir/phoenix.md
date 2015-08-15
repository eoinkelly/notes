

* Phoenix
    * top layer of the stack
    * The endpoint (note: singular)
        * handles request pre-router
        * provides a set of "plugs" (middlewares) to work on the request
        * eventually gives the request to a given router
    * The router (note singular)
        * sends request to correct controller based on params
        * provides URL generation helpers
        * lets you define "pipelines" which are groups of plugs
            * a request can be sent through a pipeline
    * controllers
        * do same stuff as in rails
    * views
        * provide helper functions to the templates
        * render the templates
    * templates
        * built into elixir (EEx)
        * look very like ERB
        * there is a Haml parser http://calliopehaml.info/
            * seems mostly done - still missing partials and exception messages
        * are precompiled into erlang functions
        * are fast because of precompilation
    * channels
        * are basically "controllers" for connections that need bi-directional persistent comms
    * pubsub
        * sits under the channels layer
        * provides an abstract pubsub adapter
            * allows you to use 3rd party pubsub services
                * e.g. ????
* Ecto
    * a pluggable database abstraction layer
    * allows you to use different databases as backends
* Plug
    * elixir middleware
    * rack for elixir
* Cowboy server
    * the server at the bottom layer
    * written in erlang
    * equivalent to unicorn/thin/webrick et. al in ruby
    * talks to higher layers via Plug interface



In .eex templates it does some metaprogramming to make `Dict.get(assigns, :messenger)` look like `@messeger`
