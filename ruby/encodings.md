# Encodings

http://graysoftinc.com/character-encodings/understanding-m17n-multilingualization

## Terminology

- character set: a mapping of symbols to "code points" (some number that
  corresponds to a symbol)
    - in Unicode they are written as U+0061
      (`U+<4 digit hex representation of the code-point number>`)
    - NOTE: this is the code point number not any particular encoding of it
    ```ruby
    # ruby can show you Unicode code-points
    "aé…".unpack("U*")
    "aé…".codepoints # same as above
    ```
- character encoding: a mapping of code-points to bytes on a computer
    - the encoded code-points are what actually gets recorded in a byte stream
- a single character set has multiple character encodings

- in the beginning there was only US-ASCII
    - it defined the first 7 bits
- as computers grew the need for more characters came around
- there was a whole unused bit in each byte of US-ASCII and "character
  encodings" were created to codify how those other 128 values should be used.
    - IMPLICATION: All character encodings are supersets of US-ASCII !
        - is defn true ???
- after a while more characters were needed so multi-byte chars became a thing
- there are many encodings in common usage today
- many different character encodings (some single byte, some multi-byte) it was
  seen as desirable to have a single character encoding for all of humanity i.e.
  Unicode
- UTF-8 is popular
    - it is 100% compatible with US-ASCII
    - unlike say UTF-32E which uses 4 bytes for every character

    ```ruby
    "abc".encode("UTF-32BE").chars
    [
    [0] "a",
    [1] "b",
    [2] "c"
    ]

    "abc".encode("UTF-32BE").bytes
    [
    [ 0] 0,
    [ 1] 0,
    [ 2] 0,
    [ 3] 97,
    [ 4] 0,
    [ 5] 0,
    [ 6] 0,
    [ 7] 98,
    [ 8] 0,
    [ 9] 0,
    [10] 0,
    [11] 99
    ]
    ```

"a".getbyte(0)

Each ruby String object has an associated Encoding object

```ruby
str = "abc"
str.encoding
str.encoding.name
```

String provides four Enumerator interfaces:

1. str.lines
2. str.codepoints
3. str.chars
4. str.bytes

and the corresponding methods for iterating over them

1. str.each_line
2. str.each_codepoint
3. str.each_char
4. str.each_byte

Note that String does not have an `#each` method!

An example

```
enc32 = "abc".encode("UTF-32BE")

enc32.lines
[
  [0] "abc"
]

enc32.chars
[
  [0] "a",
  [1] "b",
  [2] "c"
]

enc32.codepoints
[
  [0] 97,
  [1] 98,
  [2] 99
]

enc32.bytes
[
  [ 0] 0,
  [ 1] 0,
  [ 2] 0,
  [ 3] 97,
  [ 4] 0,
  [ 5] 0,
  [ 6] 0,
  [ 7] 98,
  [ 8] 0,
  [ 9] 0,
  [10] 0,
  [11] 99
]
```

- Note that only for US-ASCII characters in an encoding that is 100% US-ASCII
  compatibile will the codepoint and the byte value be the same!
- unicode code-points are a superset of latin-1
- not all unicode characters have a unique representation
    - unicode has single character versions of accented characters like é
    - unicode also has "combining marks" where the letter has one codepoint and
      the accent another and the two are shown combined when displayed
        - this is true not matter which encoding you use, even UTF-32
    - the above means that two strings which look the same visually and have the
      same encoding might have different codepoints (and hence different bytes)
      so they would not test equal!
        - QUESTION: swift does the right thing here, what does ruby do?

### Ruby 1.8

- basically supports 4 encodings
- You can tell ruby 1.8 regexps what encoding the pattern and data you pass to
  them is in
- Ruby 1.8 has $KCODE global for setting the encoding

### Ruby 1.9+

Read the currently active encoding

```
p __ENCODING__
```

Set the encoding for a source file via magic comment

```
# encoding: UTF-8
```

- In ruby 1.9.x
    - source files have default encoding of US-ASCII
    - If you use escape characters to build strings in 1.9.x ruby will silently
      change the source encoding from US-ASCII to ASCII-8BIT
    - Unicode escape `\u####` will set the String to UTF-8 regardless of current
      source encoding.

### Escapes

There are ? ways of entering arbitrary bytes in a source file

- hex escapes `\##`
- Octal escapes (\###)
- control escapes (\cx or \C-x)
- meta escapes (\M-x),
- meta-control escapes (\M-\C-x)

### Best practices for IO objects in ruby

Rule of thumb: normalize a group of String objects to the same Encoding before
working with them together. That goes for comparisons and other shared
operations as well.

??? declare the encoding when you create it? I assume it defaults to UTF-8 so
maybe only declare when you wnat to change that

What does it look like to do ruby and be 100% explicit about the encodings you
want? e.g.

```
# how to explitly tell ruby waht encoding to use for a string literal
# how to explitly tell ruby waht encoding to use for a string from user input
# how to explitly tell ruby waht encoding to use for a string from a file
# others?
TODO
```

### Ruby 2.0+

- Ruby 2.0+ source files have default encoding of UTF-8

### IO Objects: Intenal vs External encodings

When reading strings from an IO object you can set an external and internal
encodings

- external encoding
    - matches the encoding used by the IO source (string, file, network etc)
- internal encoding
    - ruby will transcode the source to this if you set it (it defaults to
      `nil`)

Ruby 2 defaults to

```ruby
# default internal encoding is nil
Encoding.default_internal # => nil

# default external encoding is UTF-8
Encoding.default_external # => #<Encoding:UTF-8>

# You can change the defaults. Encodings are available as constants under the
# `Encoding` namespace.

Encoding.default_internal = Encoding::UTF_8
Encoding.default_external = Encoding::ISO_8859_1
```

so ruby has three default encodings

1. default encoding for source files
2. default external encoding for IO objects
3. default internal encoding for IO objects

got to http://graysoftinc.com/character-encodings/miscellaneous-m17n-details

### Inspect & change encodings in Ruby

```ruby
abc = "hello"
puts abc.encoding.name

# just change encoding (leaves data unchanged)
abc.force_encoding("UTF-8")
puts abc.encoding.name
puts apc.valid_encoding? # true if the bytes in ABC are valid for its encoding

# transcode the data
abc.encode("other-encoding-name")
abc.encode!()
```

### Encoding as toolbox

- `Encoding` has a bunch of tools for helping you manage string encodings

```
Encoding.list # return array of all known encodings
Encoding.list.count # => 101 in Ruby 2.4.1
Encoding.find("utf-8") # find an encoding

# encodings can have aliases
Encoding.find("ASCII") == Encoding.find("US-ASCII") # => true

# show aliases for an encoding
Encoding.aliases # => hash of all alias mappings
Encoding.aliases["ASCII"]

# returns the name of the encoding that can be used for both, false if incompatible
Encoding.compatible?(str1, str2)
```

### Dummy encodings

From the ruby docs:

> A dummy encoding is an encoding for which character handling is not properly
> implemented. It is used for stateful encodings.

You can inspect which encodings are "dummy":

```
Encoding.list.select(&:dummy?).map(&:name)
# => ["UTF-16",
#  "UTF-32",
#  "IBM037",
#  "ISO-2022-JP",
#  "ISO-2022-JP-2",
#  "CP50220",
#  "CP50221",
#  "UTF-7",
#  "ISO-2022-JP-KDDI"]
```

QUESTION: why are UTF-16 and UTF-32 considered "dummy" QUESTION: are they
stateful in some way I don't know about?

> Some encodings are stateful; they have bytes or byte sequences that switch the
> meanings of the following bytes.

> Code sets can be classified into two categories: stateful encodings and
> stateless encodings.

> Stateful encoding uses sequences of control codes, such as shift-in/shift-out,
> to change character sets associated with specific code values.

> For instance, under compound text, the control sequence "ESC$(B" can be used
> to indicate the start of Japanese 16-bit data in a data stream of characters,
> and "ESC(B" can be used to indicate the end of this double-byte character data
> and the start of 8-bit ASCII data. Under this stateful encoding, the bit value
> 0x43 could not be interpreted without knowing the shift state. The EBCDIC
> Asian code sets use shift-in/shift-out controls to swap between double- and
> single-byte encodings, respectively. TODO: wtf?

### iconv

```
man iconv
iconv -l # show available encoding names
```

- C program
- converts text from one encoding to another
- included on mac and linux
- available encodings is system dependent
- a command line wrapper around libC `iconv_open` (man 3)

TODO: I think this can be used to test the encoding of a file by trying to
convert and see what blows up???

# Details of UTF-8 encoding

- UTF-8 is a multibyte encoding
- it uses the most significant bits of the first byte to indicate how many bytes
  this character will be

1. single byte characters always have `0xxxxxxx` as their most significant bit
2. the number of signifcant `1` bits in the first byte indicates how many bytes
   this character will take up
    - a char beginning iwth `110xxxxx` will be two byte
    - a char beginning iwth `1110xxxx` will be three byte
3. all other bytes in a multibyte sequence begin with `10xxxxxx`

```ruby
def as_bytes(str)
  str.chars.map { |c|
    c.bytes.map { |b| "%08b" % b } # use sprintf format to get padded binary representation
  }
end

[215] pry(main)> as_bytes("aß…")
[
  [0] [
    [0] "01100001"
  ],
  [1] [
    [0] "11000011",
    [1] "10011111"
  ],
  [2] [
    [0] "11100010",
    [1] "10000000",
    [2] "10100110"
  ]
]
```

### Aside: entering special chars on mac

Many common "non ascii" characters are already mapped to the keyboard using the
option key as a modifier

- Mapped via `Option-` modifier
    - a =
- Mapped via `Option-shift-` modifier
    - k = apple logo

### Aside: entering special chars in vim

- vim has its own way of doing it
- :help digraphs
- type `ctrl-k` then one of the special two char digraph codes
- `:digraphs` to see a list of currently defined digraphs

```
# example:
# type: ctrl-k n ?
ñ
```

### Aside: Setting up unicode hex input on a mac

1. Open `System Preferences > Keyboard > Input sources`
1. Add "Unicode hex input" as an input
1. Choose "Unicode hex input" from the "inputs" menu in the mac menu bar
1. open your document
1. Hold option and type 0 3 b 1 to get an alpha character

Note that while "Unicode hex input" is the active input source the usual
`Option-` and `Option-shift` modifier special chars will not work.
