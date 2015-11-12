# Keyword lists
# #############

# A common pattern is a list of 2-tuple of {atom, something}

name_values_1 = [{:name, "Eoin"}, {:age, 36}]

# elixir has sugar syntax for this pattern
name_values_2 = [name: "Eoin", age: 36]

IO.inspect name_values_1 == name_values_2 # true

# using the square bracket operator on a list will search for a
IO.inspect name_values_1[:name] # oddly this works!
IO.inspect name_values_1 ++ [{:langs, "lots"}]

IO.inspect name_values_2[:name]
IO.inspect name_values_2 ++ [langs: "lots"]
# The sugar syntax is just another way of making a List data structure so it
# has all the same performance trade-offs as other lists
# * adding = O(n)
# * prepending = O(1)

# exploring the list[:atom] sugar syntax
odd_list = [
  "hello",
  555,
  {:age, "none"},
  {"other", :legion},
  {:likes, "foo", "boo"},
  {"blah", :thinks, "things"}
]
IO.inspect odd_list[:age] # works
# IO.inspect odd_list[:likes]  # RuntimeError
# IO.inspect odd_list[:legion] # RuntimeError
# IO.inspect odd_list[:thinks] # RuntimeError

# [] function will look for an element in the list which
# * is a 2-tuple of {atom, other}
# * has the atom as the first element
# * it does not match any other elements in the list

# Important features keyword lists have that other dictionary types do not:
#
# 1. The same key can appear multiple times
# 2. The keys are _ordered_!
#
# They are the default way of passing args to functions in elixir.
# When the keyword list is the last arguments to a function the surrounding []
# are optional.
#   * where ruby makes an anonymous hash, elixir makes a keyword list

dups_list = [name: "Eoin", name: "Kelly", age: 36, age: 37]

# [] only finds the first of each duplicate key
IO.inspect dups_list[:name]
IO.inspect dups_list[:age]
IO.inspect Keyword.get_values(dups_list, :name)

# * The Keyword module provides functions to work with keyword lists
# * most of the functions in there don't deal with duplicate keys as it is not
#   a common use case

# pattern matchin keyword lists isn't really a thing because ???
# i guess because of the duplicate values ???
#

# Dictionary types
# ################

# Dictionary types must implement the following protocols in elixir
#
# * Dict
# * Access
# * Enumerable
# * Collectable

# Things which implment the "dict" behavior in Elixir 1.1
#
# 1. Map
# 2. hashDict
# 3. Keyword
#
# Dict is also a module of functions that contains the methods you use to
# interact with dictionary types in elixir
#
# Hint: Use Enum.into to convert between dict types

# Dictionary types:
#
# Elixir 1.1
#
# 1. Map (linear time with no. of keys, can be slow)
# 1. HashDict
# 1. Keyword
#
# All of the above use the Dict API
#
# Elixir 1.2
#
# Map
#     * supports pattern matching
#     * are fast in Erlang 18
#     * cannot control ordering
#
# All of these are deprecated in Elixir 1.2
#
# 1. HashDict
# 2. Dict
# 3. Set
# 4. HashSet


key_list = [name: 'John', height: 133]

IO.inspect Enum.into(key_list, HashDict.new)
IO.inspect key_list |> Enum.into HashDict.new

IO.inspect key_list |> Enum.into Map.new



# HashDict
# ########
#
# * represented internally as a struct %HashDict{}
# * implemented using "tries" which grows in space as the no. of keys grows ???

# Map
# ###
#
# * key value stores
# * created with the %{} "special form"

example_map = %{ name: "John", height: 133 }
IO.inspect Dict.values(example_map)
IO.inspect Dict.keys(example_map)

