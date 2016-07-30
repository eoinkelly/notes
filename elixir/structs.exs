# Structs
# #######
#
# * A struct is a map with a predefined set of key names
#     * will fail at compile time (unlike map) if you try to assign a key that does not exist
# * gives you structname.membername access to members
# * Struct must be defined in a module (the module name will become the struct name
#     * `defstruct` is the keyword used to introduce a struct definition
# * Structs are extensions built on top of maps that provide compile-time checks and default values.
# * Structs provide compile-time guarantees that only the fields (and all of
#   them) defined through defstruct will be allowed to exist in a struct
# * A struct is a map that has a `__struct__` key
# * you use the functions from `Map` module to work with structs
# * A struct is a "bare" map i.e. a map that doesn't have any of the map protocols impelmented
# * structs do not share protocol implementations with maps! - they require their own protocol implementation


defmodule User do
  # option 1: pass a list of field names
  defstruct [:id, :name, :age]
end

defmodule Person do
  # option 2: pass a map of field names to default values
  defstruct id: 123, name: "John doe", age: 25
end

# iex> %User{}
# %User{age: nil, id: nil, name: nil}
# iex> %Person{}
# %Person{age: 25, id: 123, name: "John doe"}
#
#
# # lets find out about the __struct__ key
# iex> i Map.get(some_struct, :__struct__)
# Term
#   Person
# Data type
#   Atom
# Module bytecode
#   :in_memory
# Source
#   iex
# Version
#   [334455153148985576654726744237587764418]
# Compile time
#   2016-2-28 17:39:6
# Compile options
#   [:debug_info]
# Description
#   Call Person.module_info() to access metadata.
# Raw representation
#   :"Elixir.Person"
# Reference modules
#   Module, Atom
