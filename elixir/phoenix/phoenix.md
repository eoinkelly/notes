# Phoenix

* Phoenix
    * top layer of the stack
    * The endpoint (note: singular)
        * handles request pre-router
        * provides a set of "plugs" (middlewares) to work on the request
        * eventually gives the request to a given router
    * The router (note: singular)
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
        * each template is precompiled into a render/2 function in the View `SomeView.render("foo.html", assigns)
        * get a Dict of variable assignments in `assigns` that they can use
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

# Phoenix Templates

* In .eex templates it does some metaprogramming to make `Dict.get(assigns,
  :messenger)` look like `@messenger`
* Each View has a directory of templates

> The way we pass data into a template is by the assigns map, and the way we
> get the values out of the assigns map is by referencing the keys with a
> preceeding @.

* `@foo` in a template refers to the `foo` key of `assigns` map
* `@conn` (the `conn` key of `assigns`) is is a ??? that represents the ???
    * it seems to be a pattern to pass @conn into helper methods as first arg

Sources of helpful functions in templates

1. `web/web.ex` defines some imports for all Views
    * from controller
        * get_csrf_token/0
        * get_flash/2
        * view_module/1
    * All of Phoenix.HTML
    * All of MyApp.Router.Helpers
        * brings in `*_path` methods similar to rails
2. functions defined in the view object are automatically available to the template

```
Phoenix.Endpoint.Adapter.static_path/2
static_path(@conn, "/css/app.css")
```

* can render arbitrary templates in templates:
    ```
    <%= render MyPhoenixApp.SomeSharedView, "template_name.html", varname_in_template: some_var %>
    ```
* To iterate across a collection in a template:
    ```
    <% for item <- some_collection do %>
    <%= item %>
    <% end %>
    ```

You can render templates from iex - the render function takes a View module, a
template name and a map of variable assignments

```
iex(2)> Phoenix.View.render(RabidsitePhoenix.SomeView, "index.html", %{})
{:safe, ["" | "contents of view"]}
```
note that

* you pass the template name as .html not .eex or .haml
* the :safe atom is how Phoenix marks templates as safe

