# Ruby and binary data

## Encodings

* Every string in ruby has an associated encoding (view it via `str.encoding`)
* Understanding encodings is foundational knowledge for handling binary data in ruby
* All Ruby script code has an associated Encoding which any String literal created in the source code will be associated to.
    * the default encoding of source files (and therefore string literals) changed in Ruby 2.0 from US-ASCII to UTF-8.
    * you can see the currently active encoding via the `__ENCODING__` constant
    * the default encoding can be changed by a magic comment on the first line of the source code file (or second line, if there is a shebang line on the first).
    ```ruby
    # encoding: UTF-8

    # the magic comment above makes all string literals created in this file use that encoding
    "some string".encoding #=> #<Encoding:UTF-8>
    ```
* IMPORTANT: strings that represent binary data should have the `Encoding::ASCII_8BIT` encoding
* `Encoding::ASCII_8BIT` is a special encoding that is usually used for a byte string, not a character string.

There are three ways to make an `Encoding::ASCII_8BIT` encoded string literal
in ruby:

```ruby
way_1 = "\xff\x01".force_encoding(Encoding::ASCII_8BIT) # "\xFF\x01"
way_1.encoding # #<Encoding:ASCII-8BIT>

way_2 = "\xff\x01".b # => "\xFF\x01"
way_2.encoding # => #<Encoding:ASCII-8BIT>

way_3 = ["ff01"].pack("H*") # => "\xFF\x01"
way_3.encoding # => #<Encoding:ASCII-8BIT>
```

## String literals with non-ascii bytes

* Use `\x` to make string literals with byte values outside the normal ASCII range
* IMPORANT: The currently active encoding (see `__ENCODING__` will be used to create the string)
    * strings with different encodings will not compare as equal for bytes
    outside of the printable ASCII ranges
    ```ruby
    "\xff" == "\xff" # => true
    "\xff" == "\xff".b # => false
    ```
* use `\x` escape character to put non-printing byte values in a ruby string
* you must use double quoted strings or the `\x` escape char will not be enabled
* case does not matter e.g. `\xAB` == `\xAb` == `\xab`

```ruby
# create a single char string with a non printing value

# option 1: (type it in directly)
"\xfe"

# option 2: (convert from Integer)
0xFE.chr # => "\xFE"
254.chr # => "\xFE"


# Putting the binary equivalent of "FE00CFAB" directly into ruby
ss = "\xfe\x00\xcf\xab"
```


### Reading binary data in/out

Each IO object has an external encoding which indicates the encoding that Ruby will use to read its data. By default Ruby sets the external encoding of an IO object to the default external encoding. The default external encoding is set by locale encoding or the interpreter -E option. ::default_external returns the current value of the external encoding.

```ruby
Encoding.default_external # #<Encoding:UTF-8> on my system
```


IO objects have an external encoding associated
    * it can be set by IO#set_encoding or by passing options to IO.new


## String#ord

* Return the Integer ordinal of a one-character string.

* converts a single char string into an integer based on its binary value
* only looks at the first char in the string

```ruby

"\xFE".ord # => 254
"\x61".ord # => 97
"\x97".ord  # ArgumentError: invalid byte sequence in UTF-8
```

When typing in literals ...

* `\x00` to `\x79` work with #ord
* `\x80` to `\xFF` raise ArgumentError

```ruby

254.chr # => "\xFE"
254.chr.ord # => 254

# why does the above work but if I type in "\xFE".ord I get ArgumentError


a = 254.chr
b = "\xfe"

a == b # => false ?????
a.bytes == b.bytes # => true

[129] pry(Encoding):1> a.encoding
#<Encoding:ASCII-8BIT>
[130] pry(Encoding):1> b.encoding
#<Encoding:UTF-8>


c = 254.chr(Encoding::UTF_8)
d = "\xfe"

[135] pry(Encoding):1> c.encoding
#<Encoding:UTF-8>
[136] pry(Encoding):1> d.encoding
#<Encoding:UTF-8>
[137] pry(Encoding):1> c == d
false

# attempting to change encoding of b results in ...

b2 = b.encode(Encoding::ASCII_8BIT)
Encoding::InvalidByteSequenceError: "\xFE" on UTF-8

so the string "\xfe" cannot be

# change encoding without transforming the data
b.force_encoding(Encoding::ASCII_8BIT)

a == b
# true
```



### Integer#chr(encoding)

* chr converts an integer into a string with the same binary value
* encodes an integer as a string according to encoding

```ruby
255.chr(Encoding::BINARY) # => "\xFF"
255.chr(Encoding::UTF_8) # => "ÿ"
```

QUESTION: what is the default encoding for ruby ???


## String#bytes

"abc".bytes

## String#each_byte

## String#chars

"abc".chars


convert Fixnum to a Sting representation of the number in the given base

    100.to_s(16)
    100.to_s(2)

note that it does NOT conver the number to its hex or binary equivalent

String#bytes

### String#hex

* hex :: String -> Fixnum
* converts a given string of chars in the hex range (0-9,a-f) into the corresponding number
* string can have optional `0x` prefix
* returns 0 if string can not be converted to hex

    "9".hex # => 9
    "a".hex # => 10
    "0xa".hex # => 10

### Converting a sting of hex bytes into a stirng of binary data

* the string represents a *stream* of numbers not a single number

```ruby
def hex_to_bin(s)
    # s.scan(/../) will break the string into pairs and ignore any leftover e.g.
    # "hello" -> ["he", "lo"]
    # hex converts the two-char string into a number in 0-255 range
    # chr converts than number into a string e.g. 255.chr --> "\xFF"
    # chr(encoding) ???
  s.scan(/../).map { |x| x.hex.chr }.join
  # the return value is a string of binary data
end
```
## Binary literals


"\x34\x45\xFF"

### Base64

* Use the "strict" versions of the encode and decode methods to not add
newlines after every 60 chars

```ruby
require "base64"

Base64.strict_encode64(data)
Base64.strict_decode64(data)
```

### Write binary string to a file

### Read binary data from a file
