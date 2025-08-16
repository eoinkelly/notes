# Variable length bit streams in Ruby

- [Variable length bit streams in Ruby](#variable-length-bit-streams-in-ruby)
    - [Strategies](#strategies)
        - [Strategy 1: Avoid bits entirely: store each bit as a byte](#strategy-1-avoid-bits-entirely-store-each-bit-as-a-byte)
        - [Strategy 2: Buffer of bytes with padding](#strategy-2-buffer-of-bytes-with-padding)
        - [Reading one bit at a time in Ruby](#reading-one-bit-at-a-time-in-ruby)
        - [Writing one bit at a time](#writing--one-bit-at-a-time)
    - [Working with a bit buffer which is not always an even multiple of 8](#working-with-a-bit-buffer-which-is-not-always-an-even-multiple-of-8)
        - [Step 1: Encoding the bit buffer as a (byte-buffer, length) tuple](#step-1-encoding-the-bit-buffer-as-a-byte-buffer-length-tuple)

* Variable length codes (VLCs) generate bit strings which are of arbitrary
  length, not necessarily ending on byte boundary.
* Each buffer is a concatenation of values of variable bit lengths with no
  delimiter between the values i.e. You need the encoding dictionary to read the
  data back out.
* You need to be able to create a buffer of **bits** which are read one at a
  time to recognise the encoded symbols

To encode a VLC you need to store:

1. the stream of bits representing the encoded symbols
2. the encoding dictionary
3. bit stream metadata
    - the length of the stream of bits (if the stream of bits has been packed
      into a stream of bytes)
    - where the padding bits have been added to pack the bit stream into a bytes
      buffer

## Strategies

### Strategy 1: Avoid bits entirely: store each bit as a byte

This is wasteful of memory but should probably be your preferred good option if
your streams aren't big - it's a lot easier to work with!

Instead of reading/writing individual bits you can choose a byte value to
represent 0 and 1. If you use this strategy then you don't need to store the
length of the bit stream as a separate value.

Options:

- Option: Represent the buffer as an `Array` and you use `0` and `1` `Integer`
  literals.
    - ++ buffer can also be visually inspected but with a bit more noise
      (commas)
    - My gut is that this is probably better but I have no evidence
- Option: Represent the buffer as a `String` and "0" (`0x30`) and "1" (`0x31`)
  as ASCII characters
    - ++ generates a buffer which can be visually inspected
    - Ruby strings are UTF-8 by default and we are storing only two values from
      the ASCII range so encodings don't matter

### Strategy 2: Buffer of bytes with padding

- Store your VLC bitstream in a `String` buffer, adding padding to either the
  start or end to get to full bytes

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

### Writing one bit at a time

## Working with a bit buffer which is not always an even multiple of 8

Encoding data as binary always assumes a **byte** buffer i.e. a buffer of bits
whose length is a multiple of 8.

If you have a bit buffer which does not fit evenly into bytes then you need an
extra level of encoding i.e. you need a two-level encoding:

1. Encode the _bit buffer_ as a _byte_ buffer and a length
2. Encode the _byte_ buffer for sending to a file or socket - see
   [my general doc on Ruby binary data](./ruby-with-binary-data.md)

### Step 1: Encoding the bit buffer as a (byte-buffer, length) tuple

To handle a bit buffer which may not always fill whole bytes e.g. 11 bits, 13
bits, you need to know the following:

1. The bits, stored in byte (8bit) size boxes
2. The length of the bits you care about OR the length of bits to skip at the
   start/end to start reading
    - num skipped bits is probably a better choice because it's always 0..7
3. Whether the padding bits (if any) appear at the start or end of the bitbuffer

This means that the bitbuffer must be "packaged" somehow to have this metadata.
The location of the padding bits does not change so could be baked into your
encoder and decoder if that is practical.

Should padding bits be added at the start or the end?

- Add them at the start
    - ignore
- Add them at the end
    - ??
