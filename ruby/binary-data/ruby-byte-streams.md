# A guide for working with binary data in Ruby

- [A guide for working with binary data in Ruby](#a-guide-for-working-with-binary-data-in-ruby)
  - [Background stuff](#background-stuff)
    - [Ruby features and gems for working with binary data](#ruby-features-and-gems-for-working-with-binary-data)
    - [Variable length codes](#variable-length-codes)
  - [Strategies](#strategies)
    - [Option 1: Avoiding bits entirely: represent each bit using a one byte value](#option-1-avoiding-bits-entirely-represent-each-bit-using-a-one-byte-value)
    - [Option 2: Buffer of bytes with padding](#option-2-buffer-of-bytes-with-padding)
    - [Reading one bit at a time in Ruby](#reading-one-bit-at-a-time-in-ruby)
    - [Writing  one bit at a time](#writing--one-bit-at-a-time)
  - [Working with a bit buffer which is not always an even multiple of 8](#working-with-a-bit-buffer-which-is-not-always-an-even-multiple-of-8)
    - [Step 1: Encoding the bit buffer as a (byte-buffer, length) tuple](#step-1-encoding-the-bit-buffer-as-a-byte-buffer-length-tuple)
    - [Step 2: Encode the (byte-buffer, length) tuple](#step-2-encode-the-byte-buffer-length-tuple)
  - [Working with a byte buffer (a bit buffer whose length which is an even multiple of 8)](#working-with-a-byte-buffer-a-bit-buffer-whose-length-which-is-an-even-multiple-of-8)
- [Tasks](#tasks)
  - [Byte aligned data](#byte-aligned-data)
    - [How do I read a binary data structure from a file?](#how-do-i-read-a-binary-data-structure-from-a-file)
    - [How do I write a binary data structure to a file?](#how-do-i-write-a-binary-data-structure-to-a-file)
  - [Unaligned data](#unaligned-data)
- [Appendices](#appendices)
  - [Appendix: Choosing endianness](#appendix-choosing-endianness)

## Background stuff

### Ruby features and gems for working with binary data

Ruby features which are relevant for working with binary data

* [Integer#[]](https://ruby-doc.org/core-3.1.2/Integer.html#method-i-5B-5D)
* [Array#pack](https://ruby-doc.org/core-3.0.0/Array.html#method-i-pack)
* [String#unpack](https://ruby-doc.org/core-3.1.2/String.html#method-i-unpack)
* [String#unpack1](https://ruby-doc.org/core-3.1.2/String.html#method-i-unpack1)
* [String#to_i(base = 10)](https://ruby-doc.org/core-3.1.2/String.html#method-i-to_i)
  * assuming the string is an integer of base `base`, try to build the integer
  * base must be in 2..36 range
  * returns 0 if it can't do the conversion
  * examples
    ```ruby
    "hello".to_i(2)
    # => 0
    "0011hello".to_i(2)
    # => 3
    * very handy for debugging
    ```
* [Integer#_to_s(base = 10)](https://ruby-doc.org/core-3.1.2/Integer.html#method-i-to_s)
  * convert the integer to a string representation in the given base
  * aliased as `Integer#inspect`
  * very handy for debugging

Ruby gems

* [bindata](https://github.com/dmendel/bindata)
* others - see https://github.com/dmendel/bindata/wiki/Alternatives (they seem to be a mix of niche feature-set and/or lightly maintained)

Examples

```ruby
# Integer#[] can extract bits from an integer
aa = 999999999999999
# => 999999999999999

aa.to_s(2)
# => "11100011010111111010100100110001100111111111111111"

aa[0,16]
# => 32767
aa[0,16].to_s(2)
# => "111111111111111"

aa[0,32].to_s(2)
# => "10100100110001100111111111111111"

```

### Variable length codes

* Variable length codes (VLCs) generate bit strings which are of arbitrary length, not necessarily ending on byte boundary.
* Each buffer is a concatenation of variable length values with no delimiter
    * You need the encoding dictionary to read the data back out
* You need to be able to create a buffer of **bits** which are read one at a time to recognise the encoded symbols

## Strategies

### Option 1: Avoiding bits entirely: represent each bit using a one byte value

* This should probably be your preferred good option if your buffers aren't big - it's a lot easier to work with
* Instead of reading/writing individual bits you can choose a byte value to represent 0 and 1.
* Option: Represent the buffer as a `String` and "0" (`0x30`) and "1" (`0x31`) as ASCII characters
    * ++ generates a buffer which can be visually inspected
    * Ruby strings are UTF-8 by default and we are storing only two values from the ASCII range so encodings don't matter
* Option: Represent the buffer as an `Array` and you use `0` and `1` `Integer` literals.
    * ++ buffer can also be visually inspected but with a bit more noise (commas)

TODO: ?? Which option is more efficient in Ruby memory for a very large buffer?

### Option 2: Buffer of bytes with padding

* Store your VLC bitstream in a `String` buffer, adding padding to either the start or end to get to full bytes

TODO: can bindata handle this well?

### Reading one bit at a time in Ruby

See [example ruby file](./code/binary.rb)

```ruby
# This is the C style implementation with bit shifting
# @param [String] buffer
# @return [Array<Integer>]
def extract_bits_to_array_of_ints_1(buffer)
	# output could instead be a String and we push "0" and "1" chars onto it
	output = []
	indexes = 7.downto(0) # could be constant

	# using #each here instead of #map or #reduce to minimise memory allocations
	buffer.bytes.each do |byte|
		indexes.each do |i|
			output.push((byte >> i) & 0b0000_0001)
		end
	end

	output
end
```

### Writing  one bit at a time


## Working with a bit buffer which is not always an even multiple of 8

Encoding data as binary always assumes a **byte** buffer i.e. a buffer of bits whose length is a multiple of 8.

If you have a bit buffer which does not fit evenly into bytes then you need an extra level of encoding i.e. you need a two-level encoding:

1. Encode the _bit buffer_ as a _byte_ buffer and a length
2. Encode the _byte_ buffer for sending to a file or socket

### Step 1: Encoding the bit buffer as a (byte-buffer, length) tuple

To handle a bit buffer which may not always fill whole bytes e.g. 11 bits, 13 bits, you need to know the following:

1. The bits, stored in byte (8bit) size boxes
2. The length of the bits you care about OR the length of bits to skip at the start/end to start reading
    * num skipped bits is probably a better choice because it's always 0..7
3. Whether the padding bits (if any) appear at the start or end of the bitbuffer

This means that the bitbuffer must be "packaged" somehow to have this metadata. The location of the padding bits does not change so could be baked into your encoder and decoder if that is practical.

Should padding bits be added at the start or the end?

* Add them at the start
  * ignore
* Add them at the end
  *  ??

### Step 2: Encode the (byte-buffer, length) tuple

You options here include:

* Pack the bit buffer and length into a binary structure
    * using https://github.com/dmendel/bindata seems like a good fit
* Use a text document structure like JSON or YAML.
    * You will probably need to base64 encode the raw bytes of the bit buffer
        * base64 encoding increases the storage required by up to 25%
        * this may matter if your encoded bits were intended to be a compressed representation of your data

## Working with a byte buffer (a bit buffer whose length which is an even multiple of 8)

* just do step 2 above
* Use https://github.com/dmendel/bindata


=============================

# Tasks

TODO: is "byte aligned" the correct term here?

## Byte aligned data

### How do I read a binary data structure from a file?

Use: https://github.com/dmendel/bindata

### How do I write a binary data structure to a file?

Use: https://github.com/dmendel/bindata

## Unaligned data

# Appendices

## Appendix: Choosing endianness

If you store a value across multiple bytes (e.g. a number stored as a 64bit
integer is stored across 8 bytes), then it may dismay you to know that not all
computer systems agree on the order of the **bytes**

For example if you store the number 1 as a 64 bit Integer, some systems will store it into memory as

    # big endian (intuitive to read)
    00 00 00 00  00 00 00 01

and others will store it as

    # little endian (not intuitive to read)
    01 00 00 00  00 00 00 00

Big endian is easier to read because the number reads the way we would write it.

The CPU is the part of a computer which decides its endianness. Little endian is a defacto default in modern architectures but big endian is the default for network protocols.

| CPU Architecture                                          | Endianness        |
| --------------------------------------------------------- | ----------------- |
| Intel CPUs (e.g Windows)                                  | Little endian     |
| AMD CPUs (e.g Windows)                                    | Little endian     |
| ARM CPUs (e.g. Apple Silicon, Raspberry Pi, AWS Graviton) | Little endian[^1] |
| Network byte order (e.g. TCP)                             | Big endian        |

[^1]: Some ARM CPUs can be configured to be either endianness but little is default for Linux


More info on this faff at the [Wikipedia entry](https://en.wikipedia.org/wiki/Endianness) or this [YouTube explainer](https://www.youtube.com/watch?v=NcaiHcBvDR4).


If you are confident that encoding and decoding will always happen on the same
type of CPU then you can ignore endianness and just default to whatever your
platform uses.

If encoding and decoding might happen on different machines with different
endianness then you should pick a specific endianness and stick with it.

If you are creating a network protocol, choose big endian because it is more conventional in that space.

If you aren't sure which endianness to pick, choose big endian because it seems it is more conventional in file formats according to
the [bindata gem docs](https://github.com/dmendel/bindata/wiki/PrimitiveTypes) say:

> These integers are used to define bitfields in records. Bitfields default to
> unsigned and big endian, but signed and little endian may be specified
> explicitly. Little endian bitfields are rare, but do occur in older file formats
> (e.g. The file allocation table for FAT12 filesystems is stored as an array of
> 12bit little endian integers).
