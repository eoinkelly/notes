# Projectionist.vim

* `vim-rails` contains a bunch of pre-defined projections for Rails apps
    * `Einitializer` with no arg edits the routes file
* Put a `.projections.json` in your project root to customise projections for your project

```javascript
// Handy Rails projections which are not included in vim-rails
{
  "config/routes.rb": { "type": "routes" }
}
```

```javascript
// an example .projections.json for Rails (duplicates stuff in vim-rails)
{
  "app/models/*.rb": {
    "type": "model",
    "alternate": "spec/models/{}_spec.rb"
  },
  "app/services/*.rb": {
    "type": "service",
    "alternate": "spec/services/{}_spec.rb"
  }
}
```

Elixir

```json
{
  "lib/*.ex": {
    "type":      "src",
    "alternate": "test/{}_test.exs"
  },
  "test/*_test.exs": {
    "type":      "test",
    "alternate": "lib/{}.ex"
  }
}
```

Elixir & Phoenix

```json
{
  "web/models/*.ex": {
    "type": "model",
    "template": [
      "defmodule NameOfProject.{camelcase} do",
      "  @moduledoc \"\"\"",
      "  \"\"\"",
      "",
      "  use NameOfProject.Web, :model",
      "end"
    ]
  },
  "web/controllers/*_controller.ex": {
    "type": "controller",
    "template": [
      "defmodule NameOfProject.{camelcase}Controller do",
      "  @moduledoc \"\"\"",
      "  \"\"\"",
      "",
      "  use NameOfProject.Web, :controller",
      "end"
    ]
  },
  "web/views/*_view.ex": {
    "type": "view",
    "template": [
      "defmodule NameOfProject.{camelcase}View do",
      "  @moduledoc \"\"\"",
      "  \"\"\"",
      "",
      "  use NameOfProject.Web, :view",
      "end"
    ]
  },
  "web/templates/*.html.eex": {
    "type": "template"
  },
  "web/channels/*_channel.ex": {
    "type": "channel",
    "template": [
      "defmodule NameOfProject.{camelcase}Channel do",
      "  @moduledoc \"\"\"",
      "  \"\"\"",
      "",
      "  use NameOfProject.Web, :channel",
      "",
      "  def join(\"{plural}:\" <> subtopic, _parms, socket) do",
      "    {open}:ok, assigns(socket, :subtopic, subtopic){close}",
      "  end",
      "end"
    ]
  },
  "web/channels/*_socket.ex": {
    "type": "socket",
    "template": [
      "defmodule NameOfProject.{camelcase}Socket do",
      "  @moduledoc \"\"\"",
      "  \"\"\"",
      "",
      "  use Phoenix.Socket",
      "",
      "  ## Channels",
      "  channel \"{plural}:*\", NameOfProject.{camelcase}Channel",
      "",
      "  ## Transports",
      "  transport :websocket, Phoenix.Transports.WebSocket",
      "  # transport :longpoll, Phoenix.Transports.LongPoll",
      "",
      "  def connect(_params, socket) do",
      "    {:ok, socket}",
      "  end",
      "",
      "  def id(_socket), do: nil",
      "end"
    ]
  },
  "web/router.ex": {
    "type": "router"
  },
  "web/web.ex": {
    "type": "web"
  },
  "priv/repo/migrations/*.exs": {
    "type": "migration"
  },
  "priv/repo/seeds.exs": {
    "type": "seed"
  },
  "web/static/js/*.js": {
    "type": "js",
    "template": [
      "let {capitalize} = {open}",
      "{close}",
      "export default {capitalize}"
    ]
  },
  "web/static/css/*.css": {
    "type": "css"
  }
}
```
