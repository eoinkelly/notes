# Programming Phoenix book

Possible future side topics to research

- how live reloading works
- erlang archives

- `connection` is a struct (map with known fields) that represents the HTTP
  request
- phoenix can be visualised as a pipeline that takes the `connection` in at one
  end and pumps out a `connection` that contains the HTTP response as well as
  the HTTP request
- every layer of phoenix is just a thing that takes a connection and returns a
  slightly different connection
    - the first argument of each function is a connection ( this makes it play
      nicely with `|>`)

phoenix is

```
connection |> endpoint |> router |> pipelines |> controller
```

controller is

```
connection |> controller |> common_services |> action
```

an action might be

```
connection |> find_user |> view |> template
```

- phoenix tries to limit functions that have side-effects to the `controller`
    - so model and view function should be pure
    - examples
        - controller is responsible for "getting" data from the DB
        - controller is responsible for getting data from another web server
        - the models are responsible for "processing" it
        - controller is responsible for "writing" data to the DB
- phoenix likes to use string keys for external "unsafe" data but use atom keys
  when passing data between internal functions

A phoenix app is a collection of plugs

`Plug.Conn` is the common data structure passed aroudn between Plug plugins It
represents the whole request and response:

- inbound request
- protocol
- parsed parameters

Attributes of plugs

- plugs are functions of type `plug -> plug`
- each plug takes a `conn` and returns a slightly different `conn`
- the web server provides the first `conn` instance
- rendering a response is just a function (aka action) that takes a `conn` and
  transforms it to a string that the web server can send back to the browser
- plugs can be made up of other plugs

### /web vs /lib

- code in /web
    - is automatically reloaded
    - is for your web app (controllers, views, models etc.)
- code in /lib
    - is not automtatically reloaded
    - is for long running services e.g. supervision trees

Phoenix does not limit you to running your app on a single port - you can bind
different bits of it to different ports e.g. main app on 80 and admin app on
5555

The `web.ex` file will be macro-expanded into every view so don't add too much
crap to it

Phoenix infers the name of the view from the controller name The view infers the
template directory from the name of the view Phoenix uses singular names
throughout avoiding pluralisation bullshit

When it compiles phoenix will take the template files from disk and compile them
into functions that it adds to the view module

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

- when you invoke `render` in a controller you are actually rendering a layout

Introspection

```
Foo.Bar.module_info # get info about the module
h Foo.Bar.some_func # see the help docs about a function
```

Repo

- the repo is the process that does database access
- it is started by and supervised by the application process (see
  lib/appname.ex)

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

Repo.all/1 Repo.get/2 Repo.get_by/2

for example

Repo.all(User) Repo.get(User, 1) Repo.get_by(User, name: "Foo")
