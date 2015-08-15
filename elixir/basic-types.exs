# Unicode
# #######

# * Unicode assigns each glyph a "code point" which is a positive integer
#     * Use `?x` to get the code point of a particular glyph
# * Each code point needs to be represented as a binary number - _how_ that
#   happens is specified by the "encoding"
# * UTF-8 is an _encoding_ i.e. a way of turning a list of codepoints into bytes
# * UTF-8 uses 1 byte for a codepoint if it can and 2 bytes if necessary
#     * => it is a variable length encoding

# Strings
# #######

# * Elixir has "bitstrings" - arbitrary length collections of bits
# * A "binary" is a bitstring where the no. of bits is divisible by 8
# * A "string" is a binary where the bytes represent a UTF-8 encoding of the
#   Unicode codepoints.
# * => Elixir strings are "UTF-8 encoded binaries"
#
# The "weird l" has code point 322 so cannot be represented as one byte
#
# iex(1)> ex = "hełło"
# "hełło"
# iex(2)> byte_size ex
# 7
# iex(3)> String.length ex
# 5
# iex(4)> String.codepoints ex
# ["h", "e", "ł", "ł", "o"]

# Double quoted strings are prefered in elixir - generally people only use
# single quoted strings when interfacing with Erlang

IO.inspect ?ł # => 322
IO.inspect to_char_list("I am a string")
IO.inspect is_binary("I am a string") # => true

# Char lists
# ##########
#
# * is a list of code point integers
# * is delimited by single quotes
# * will display it as the glyphs not the codepoints as long as all the
#   codepoints are within the 0-255 range
# * are useful when interfacing with Erlang (strings are lists in Erlang)

IO.inspect to_string('I am a char list')
IO.inspect is_binary('I am a char list') # => false

# atoms
# #####

# * atoms in elixir are like symbols in ruby

# all valid atom names
IO.inspect :atom_name
# IO.inspect :@also_valid_atom_name # syntax error. not according to the book???
IO.inspect :is_binary?
IO.inspect :===
IO.inspect :"func/4"

# Regex
# #####

# syntax is `~r{}`
IO.inspect Regex.split ~r{[aeiou]}, "caterpillar"
# => ["c", "t", "rp", "ll", "r"]

# ## Compound data structures
#
# {} is a tuple - closest to a ruby array (despite syntax)
#
# %{} is a map (hash)
#
#     ek = %{ :name => "Eoin", :age => "36" }
#     ek[:name] # works
#     ek.name # special syntax for atom keys
#
# [] is a list (linked list) not an array!

# Booleans
# ########

# `true` is alias for `:true` (`false` and `nil` also have symbol aliases)
IO.inspect :true === true
IO.inspect false === :false
IO.inspect nil === :nil

# anything not falsy (false, :false, nil, :nil) is truthy
#
#     ===   # strict equal
#     !==   # strict not equal
#     ==    # value equality
#     ==    # value inequality

# boolean operators: and or not
# relaxed boolean operators: && || !


