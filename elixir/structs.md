# Structs

* A struct is a "bare" map i.e. a map that doesn't have any of the map protocols impelmented
* A struct is a map that has a `__struct__` key
* a map with a predefined set of key names
    * will fail at compile time (unlike map) if you try to assign a key that does not exist
* gives you structname.membername access to members
* must be defined in a module with `defstruct`
* use functions from `Map` module to work with structs

```elixir
# make a struct
defmodule Foo
  defstruct [:id, :name, :age] # option 1: field names
  defstruct id: 123, name: "John doe", age: 25, #option 2 field names with default values
end
```

