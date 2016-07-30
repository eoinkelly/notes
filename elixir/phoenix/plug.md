# Plug

Sources

* Source code: <https://github.com/elixir-lang/plug/blob/v1.1.6/lib/plug/conn.ex#L3>
* Hex docs: <https://hexdocs.pm/plug/readme.html>

Phoenix endpoints, routers, controllers are all just plugs (that contain other plugs) e.g. each controller action is a function plug but the controller as a whole is a plug which contains a collection of plugs

* Plug is ...
    * a spec
    * an abstraction layer between web apps and web servers
* Ruby rack keeps the idea of request and response separate
* Plug unifies the request and response into a `connection`

There are two types of plug

1. function plugs
    * The "plug contract" is that  it is a function that takes an instance of `%Plug.Conn` and a map and returns a %Plug.Conn
    > In order to act as a plug, a function simply needs to accept a connection
    > struct (`%Plug.Conn{}`) and options. It also needs to return the connection
    > struct. Any function that meets those criteria will do
2. module plugs
    * implements `init/1` and `call/2`
    * `init/1` initializes any args to be passed to call/2
    * `call/2` is just a function plug


controller actions also follow the plug contract

you can use plugs directly from within a controller module
a plug is just a function that takes a conn and map so any function in the controller can be wired up with `plug :func_name`
    i.e. plugs are how you do before filters in phoenix

a plug can halt the chain by calling `halt`

## Module plug

Module plug Example

```elixir
# https://github.com/elixir-lang/plug
# https://hexdocs.pm/plug/readme.html

defmodule EoinAwesomePlug do
  # import all the main functions for working with the %Plug.Conn struct
  # https://hexdocs.pm/plug/Plug.Conn.html
  import Plug.Conn

  # init/1 is run once when the app boots
  def init(options) do
    IO.puts "hi from init/1"
    IO.puts options

    "I am return val from init/1"
  end

  # call/2 is called every request
  # it must take a %Plug.Conn and a map and return a %Plug.Conn
  # remember that elixir structs are immutable so every change to conn results in a new copy
  # * a conneciton is a *direct* conneciton to the underlying web server
  # the "opts" passed here is the return value of init/1
  def call(conn, opts) do
    IO.puts "hi from call/2"
    IO.puts opts
    # IO.puts(IO.inspect(conn))
    conn
  end
end
```

## %Plug.Conn

* %Plug.Conn
    * has a `before_send` field which is a list of callbacks to invoke before sending the response
        * callbacks are invoked in the reverse order they are registered
        * these callbacks allow a plug to take action at a point other than where it itself is registered
    * implements the Collectable and Inspect protocols
