

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


# Phoenix Templates

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

Phoenix.Endpoint.Adapter.static_path/2
static_path(@conn, "/css/app.css")

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

* @inner
    * is special in layout views
    * causes the layout view to assign the rendered contents of the
      template
    * for HTML templates @inner is always marked as safe

You can render templates from iex

```
iex(2)> Phoenix.View.render(RabidsitePhoenix.SomeView, "index.html", %{})
{:safe, ["" | "contents of view"]}
# note that
# * you pass the template name as .html not .eex or .haml
# * the :safe atom is how Phoenix marks templates as safe
```

QUESTION: how to do if statements



# Phoenix book

* `connection` is a struct (map with known fields) that represents the HTTP request
* phoenix can be visualised as a pipeline that takes the `connection` in at one end and pumps out a `connection` that contains the HTTP response as well as the HTTP request
* every layer of phoenix is just a thing that takes a connection and returns a slightly different connection
    * the first argument of each function is a connection ( this makes it play nicely with `|>`)

phoenix is

    connection |> endpoint |> router |> pipelines |> controller

controller is

   connect |> controller |> common_services |> action

an action might be

    connection |> find_user |> view |> template

* phoenix tries to limit functions that have side-effects to the `controller`
    * so model and view function should be pure
    * examples
        * controller is responsible for "getting" data from the DB
        * controller is responsible for getting data from another web server
        * the models are responseible for "processing" it
        * controller is responseible for "writing" data to the DB
* phoenix likes to use string keys for external "unsafe" data but use atom keys when passing data between internal functions

A phoenix app is a collection of plugs

`Plug.Conn` is the common data structure passed aroudn between Plug plugins
It represents the whole request and response
    * inbound request
    * protocol
    * parsed parameters
* plugs are functions
* each plug takes a `conn` and returns a slightly different `conn`
* the web server provides the first `conn` instance
* rendering a response is just a function (aka action) that takes a `conn` and transforms it to a string that the web server can send back to the browser
* plugs can be made up of other plugs

/web vs /lib

* code in /web
    * is automatically reloaded
    * is for your web app (controllers, views, models etc.)
* code in /lib
    * is not automtatically reloaded
    * is for long running services e.g. supervision trees

Phoenix does not limit you to running your app on a single port - you can bind different bits of it to different ports e.g. main app on 80 and admin app on 5555


The `web.ex` file will be macro-expanded into every view so don't addd too much crap to it

Phoenix infers the name of the view from the controller name
The view infers the template directory from the name of the view
Phoenix uses singular names throughout avoiding pluralisation bullshit

When it compiles phoenix will take the template files from disk and compile them into functions that it adds to the view module


```elixir
/web/templates/foo/thing.html

defmodule App.FooView
  # each template becomes a render/2 function head and phoenix uses pattern
  # matching to know which one to render
  def render("thing.html", assigns) do
    "i am the template output"
  end
end
```

Layouts

* layouts are just templates with a special `@inner` variable
* when you invoke `render` in a controller you are actually rendering a layout


Introspection

```
Foo.Bar.module_info # get info about the module
h Foo.Bar.some_func # see the help docs about a function
```

Repo

* the repo is the process that does database access
* it is started by and supervised by the application process (see lib/appname.ex)


```
%Rumbl.User{
    __meta__: #Ecto.Schema.Metadata<:loaded>
    id: 1
    name: "Eoin"
    password: nil
    username: "eoinkelly"
    password_hash: "blah"
    inserted_at: #Ecto.DateTime<2015-11-20T02:15:03Z>
    updated_at: #Ecto.DateTime<2015-11-20T02:15:03Z>
}
```

The Repo interface is

Repo.all/1
Repo.get/2
Repo.get_by/2

for example

Repo.all(User)
Repo.get(User, 1)
Repo.get_by(User, name: "Foo")
