# Protocols

# * allows us to send multiple types to a method and have elixir automatically pick
#   the implementation to use based on the type.
# * For built-in types we could replicate this with a function that has one head
#   per type (checked in a guard clause) but this wouldn't work for structs.
# * Passing a data type that does not implement the protocol raises an error
# * by setting `@fallback_to_any` to true we tell elixir to use the implementation from the `Any` type if it can't find an implementation for the specific type passed. This means the protocol will be available on all types (unless they have their own implementation that throws an error)

defprotocol Blank do
  @doc "Returns true if data considered blank/empty"
  def blank?(data)
end

defimpl Blank, for: Integer do
  def blank?(0), do: true
  def blank?(_), do: false
end

defimpl Blank, for: List do
  # only empty lists are considered blank
  def blank?([]), do: true
  def blank?(_), do: false
end

defmodule User do
  defstruct [:id, :name, :age]
end

defimpl Blank, for: User do
  def blank?(%User{id: nil, name: nil, age: nil}), do: true
  def blank?(_), do: false
end

# iex(17)> Blank.blank? []
# true
# iex(18)> Blank.blank? [:x]
# false
# iex(20)> Blank.blank? 33
# false
# iex(21)> Blank.blank? 0
# true

# iex(24)> uu = %User{}
# %User{age: nil, id: nil, name: nil}
# iex(25)> Blank.blank? uu
# true

# @fallback_to_any (a magical way of falling back to the Any implemnetation)
# #######################################
#
# Blank2 sets @fallback_to_any
# * this means that when elixir can't find an explicit implemenation of Blank2
#   for a particular type it will fallback to an implemenation of the protocol for
#   the `Any` type e.g.
# * fallback_to_any is "opt out" i.e. all datatypes now implement Blank2 unless
#   they provide their own implementation of it

defprotocol Blank2 do
  @doc "Returns true if data considered blank/empty"
  def blank?(data)
  @fallback_to_any true
end

# define an implemenation of Blank2 that can be used with any type (represented by the Any type)
defimpl Blank2, for: Any do
  def blank?(_), do: false
end

# @derive (the less magical way to fallback to the Any implementation of a protocol)
# ######################################

# * ++ each type has to opt-in to getting the Any implemnetation so is not so magical

# Given a protocol and an implementation for Any ...

defprotocol Blank3 do
  @doc "Returns true if data considered blank/empty"
  def blank?(data)
end

defimpl Blank3, for: Any do
  def blank?(_), do: false
end

# ... when we create a new struct we can tell it to "derive" the Blank3
#     protocol i.e. opt-in to using the Any implementation of that protocol.
#
# * ++ this is more explicit and less automagical than `@fallback_to_any

defmodule User2 do
  @derive Blank3
  defstruct name: "Eoin", age: 36
end

# Built-in protocols
# ##################

# * Enumerable

# * String.Chars
# 	* says how to convert any data structure with characters into a string
#   * exposed via to_string/1

# iex(26)> to_string :foo
# "foo"

# * Inspect
# 	* exposed via the inspect/1
# 	* used to transform any data structure into a readable textual representation
# * by convention, whenever the inspected value starts with #, it is representing
#   a data structure in non-valid Elixir syntax. This means the inspect protocol is
#   not reversible as information may be lost along the way

# iex> inspect &(&1+2)
# "#Function<6.71889879/1 in :erl_eval.expr/5>"


# Protocol consolidation
# ######################

# * since a protocol can dispatch to any data type, the protocol must check on every
#   call if an implementation for the given type exists. This may be expensive
# * after our project is compiled using a tool like Mix, we know all modules that
#   have been defined, including protocols and their implementations. This way, the
#   protocol can be consolidated into a very simple and fast dispatch module
# * From Elixir v1.2, protocol consolidation happens automatically for all
#   projects
