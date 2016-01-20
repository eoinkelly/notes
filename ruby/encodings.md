# Encodings

http://graysoftinc.com/character-encodings/understanding-m17n-multilingualization

## Terminology

* character set: a mapping of symbols to "code points" (some number that corresponds to a symbol)
    * in Unicode they are written as U+0061 (`U+<4 digit hex representation of the code-point number>`)
    * NOTE: this is the code point number not any particular encoding of it
    ```ruby
    # ruby can show you Unicode code-points
    "aé…".unpack("U*")'
    ```
* character encoding: a mapping of code-points to bytes on a computer
    * the encoded code-points are what actually gets recorded in a byte stream
* a single character set has multiple character encodings

* in the beginning there was only US-ASCII
    * it defined the first 7 bits
* as computers grew the need for more characters came around
* there was a whole unused bit in each byte of US-ASCII and "character encodings" were created to codify how those other 128 values should be used.
    * IMPLICATION: All character encodings are supersets of US-ASCII !
        * is defn true ???
* after a while more characters were needed so multi-byte chars became a thing
* there are many encodings in common usage today
* many different character encodings (some single byte, some multi-byte) it was seen as desirable to have a single character encoding for all of humanity i.e. Unicode
* UTF-8 is popular
    * it is 100% compatible with US-ASCII
    * unlike say UTF-32E which uses 4 bytes for every character
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

* Note that only for US-ASCII characters in an encoding that is 100% US-ASCII compatibile will the codepoint and the byte value be the same!
* unicode code-points are a superset of latin-1
* not all unicode characters have a unique representation
    * unicode has single character versions of accented characters like é
    * unicode also has "combining marks" where the letter has one codepoint and the accent another and the two are shown combined when displayed
        * this is true not matter which encoding you use, even UTF-32
    * the above means that two strings which look the same visually and have
      the same encoding might have different codepoints (and hence different
      bytes) so they would not test equal!
        * QUESTION: swift does the right thing here, what does ruby do?


### Ruby 1.8

* basically supports 4 encodings
* You can tell ruby 1.8 regexps what encoding the pattern and data you pass to
  them is in
* Ruby 1.8 has $KCODE global for setting the encoding

### Ruby 1.9+

Read the currently active encoding

```
p __ENCODING__
```

Set the encoding for a source file via magic comment

```
# encoding: UTF-8
```

* Ruby 1.9 source files have default encoding of USASCII
* Ruby 2.0+ source files have default encoding of UTF-8

When reading strings from an IO object you can set an external and internal encodings

* external encoding
    * matches the encoding used by the IO source (string, file, network etc)
* internal encoding
    * ruby will transcode the source to this if you set it

You can set the defaults yourself via

    Encoding.default_internal
    Encoding.default_external

so ruby has three defautl encodings

1. default encoding for source files
2. default external encoding for IO objects
3. default internal encoding for IO objects

got to http://graysoftinc.com/character-encodings/miscellaneous-m17n-details

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

# returns the name of the encoding that can be used for both, false if incompatible
Encoding.compatible?(str1, str2)
```

Rule of thumb: normalize a group of String objects to the same Encoding before working with them together. That goes for comparisons and other shared operations as well.
### iconv

```
man iconv
iconv -l # show available encoding names
```

* C program
* converts text from one encoding to another
* included on mac and linux
* available encodings is system dependent
* a command line wrapper around libC `iconv_open` (man 3)

TODO: I think this can be used to test the encoding of a file by tring to convert and see what blows up???

# Details of UTF-8 encoding

* UTF-8 is a multibyte encoding
* it uses the most significant bits of the first byte to indicate how many bytes this character will be

1. single byte characters always have `0xxxxxxx` as their most significant bit
2. the number of signifcant `1` bits in the first byte indicates how many bytes this character will take up
    * a char beginning iwth `110xxxxx` will be two byte
    * a char beginning iwth `1110xxxx` will be three byte
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

* Mapped via `Option-` modifier
    * a =
* Mapped via `Option-shift-` modifier
    * k = apple logo

### Aside: entering special chars in vim

* vim has its own way of doing it
* :help digraphs
* type `ctrl-k` then one of the special two char digraph codes
* `:digraphs` to see a list of currently defined digraphs

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
