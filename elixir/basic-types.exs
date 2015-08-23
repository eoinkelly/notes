# helper function (see anonymous-functions.exs for details)
header = fn msg -> IO.puts "\n** #{msg} **\n" end

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
header.("Strings")

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

# Exlixir convention on naming functions that count things
#
# * size => constant time
# * length => linear time

IO.inspect byte_size "foo"
IO.inspect String.length "foo"

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


long_str = """
I am a multiline
string - also known as a "heredoc"
"""
IO.puts long_str

# Sigils
# ######

# elxir has a number of sigils that can be used to creat things.
# * similar to ruby %r, %w, %W etc.
#
# ~r creates a regex
# ~w creates a list of works
# ~S is the elixir sigil for creating a heredoc strings without having to quote
#    " that appears within it

# atoms
# #####
header.("Atoms")

# * atoms in elixir are like symbols in ruby

# all valid atom names
IO.inspect :atom_name
# IO.inspect :@also_valid_atom_name # syntax error. not according to the book???
IO.inspect :is_binary?
IO.inspect :===
IO.inspect :"func/4"

# Regex
# #####
header.("Regex")

# syntax is `~r{}`
IO.inspect Regex.split ~r{[aeiou]}, "caterpillar"
# => ["c", "t", "rp", "ll", "r"]

# Tuples
# ######
header.("Tuples")

# * tuples are the closest thing to a ruby array in elixir (despite syntax)
# * are hetregenous
# * elements are stored contigiously in memory
#     * => lookup by index and getting tuple size is fast
#     * => changing the size of a tuple (by appending or prepending) is slow

IO.inspect {} # an empty tuple
IO.inspect {:ok, "Some return value"} # 2-tuple of atom, string
IO.inspect tuple_size {:ok, "Some return value", 23} # 3-tuple of atom, string, number

# Map/Dictionay/Hash
# ##################
header.("Maps")

# %{} is a map (hash)

# basic syntax
IO.inspect ek = %{ :name => "Eoin", :age => "36" }
IO.inspect ek[:name] # works
IO.inspect ek.name # special syntax for atom keys


# Lists
# #####
header.("Lists")

# * lists in elixir are linked lists (even though syntax makes them look like ruby array
# * this has implications for what operations are fast on elixir lists
# * prepending to a list is fast
# * appending to a list is slow

# lists are hetreogenous
example_list = [:a, :b, "c", 23]

IO.inspect length(example_list)
IO.inspect example_list ++ ["Hi", :there]
IO.inspect example_list -- [:a]

IO.inspect [:new] ++ example_list # fast to prepend
IO.inspect example_list ++ [:new] # slow to append

# elixir will display lists of numbers as characters if they are ALL in the
# printable range - this is a holdover from erlang
IO.inspect [104, 101, 108, 108, 111]


# Booleans
# ########
header.("Booleans")

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


