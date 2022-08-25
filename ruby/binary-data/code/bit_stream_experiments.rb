class BitStreamPacker
  REQUIRED_CHUNK_LEN = 8

  class << self
    ##
    # There are a few ways the raw stream of bits could be represented:
    #
    # * option: Array<0|1>
    # * option: Array<"0"|"1">
    # * option: String with only "0"|"1" chars
    # * option: Integer just one big number - this is the most space efficient
    #   but would be fiddly to create in Ruby
    #
    # For simplicity we only accept `Array<0|1>` but this could easily be
    # changed to accept the others.
    #
    # Padding bits are added to the **end** of the stream to make it fit into bytes
    #
    # @param [Array<0|1>] input_stream The stream of bits to pack, each bit represented with either 0 or 1
    # @return [[String, Integer]] Returns a tuple (Array) of the input stream
    #   packed into a String buffer and the length of the input stream in bits.
    #
    def pack(input_stream)
      # the output string must have a binary encoding because we will be pushing
      # arbitary bytes onto it which might not be valid UTF-8
      buffer = String.new("", encoding: Encoding::BINARY)

      input_stream.each_slice(8) do |byte_chunk|
        # e.g. byte_chunk = [0, 1, 1, 1, 0, 1, 0, 0]

        actual_chunk_len = byte_chunk.length

        # The final byte_chunk will need padding if it is less than 8 long
        if actual_chunk_length < REQUIRED_CHUNK_LEN
          num_padding_bits = REQUIRED_CHUNK_LEN - actual_chunk_len
          byte_chunk += Array.new(num_padding_bits, 0)
        end

        output << byte_chunk.map(&:to_s).join.to_i(2)
      end

      [buffer, input_stream.length]
    end
  end
end

module BitStreamExperiments
  # converts String of "0", "1" to packed String buffer and back again
  class Exp1
    def self.binary_chars_to_bits(input_str)
      # TODO: I'm sure this could be more efficient
      # first problem is how to convert this string of 0|1 chars into integers

      # pack length as a 64bit native endian unsigned int
      # TODO: should probably specify an endianness
      bits_len = [input_str.length].pack("Q*")
      chunks = input_str.reverse.chars.each_slice(8).to_a.reverse.map(&:reverse)
      chunks_a = chunks.map(&:join)
      chunks_b = chunks_a.map { |a| a.to_i(2) }
      bin_str = chunks_b.pack("C*")

      [bits_len, bin_str]
    end

    # Assumption: Any extra padding bits are at the start of the stream
    def self.bits_to_binary_chars(bits_str, bits_len)
      unpacked_ints = bits_str.unpack("C*") # array of integers

      _unpacked_len = bits_len.unpack("Q*")
      # TODO: not using bits_len much here

      # unpack the array of integers into strings of 0|1
      bin_strings = unpacked_ints.map { _1.to_s(2) }

      # need to left-pad the binary strings except the first one (any leading 0's in the first entry are padding bits we want to ignore
      yy = [bin_strings.shift]
      bin_strings.each do |str|
        # pad the str out iwth leading 0's up to 8 char width
        # push it onto our new array

        yy << str.rjust(8, "0")
      end

      yy.join
    end
  end

  class Exp2
    # This is the C style implementation with bit shifting
    # @param [String] buffer
    # @return [Array<Integer>]
    def self.extract_bits_to_array_of_ints_1(buffer)
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

    # This is a much more Rubyish implementation.
    # Are there downsides to it? Probably more copying of data?
    def self.extract_bits_to_array_of_ints_2(buffer)
      buffer.bytes.flat_map { |c| c.ord.to_s(2).rjust(8, "0").chars.map(&:to_i) }
    end

    # using Integer#[]
    # @param [String] buffer
    def self.extract_bits_to_array_of_ints_3(buffer, bits_len)
      raw_unpacked = buffer.unpck("C*") # :: Array<Integer>
      # the last value in raw_unpacked needs tohave it's padding removed
      num_padding_bits = bits_len
    end

    ##
    #
    # This puts the padding at the end of the buffer.
    # Returns the buffer of bytes and the lenght of bits
    def self.write_array_of_ints(bit_ints)
      # this string must have a binary encoding because we will be pushing arbitary bytes onto it which might not be valid UTF-8
      output = String.new("", encoding: Encoding::BINARY)

      bit_ints.each_slice(8) do |byte_chunk|
        # e.g. byte_chunk = [0, 1, 1, 1, 0, 1, 0, 0]

        # last byte_chunk needs padding if less than 8 long
        # TODO: do the padding bits thing
        required_chunk_len = 8 # could be const
        actual_chunk_len = byte_chunk.length
        if byte_chunk.length < required_chunk_len
          num_padding_bits = required_chunk_len - actual_chunk_len
          byte_chunk += Array.new(num_padding_bits, 0)
        end

        # Ruby way #1
        byte_val_ruby_1 = byte_chunk.map(&:to_s).join.to_i(2)

        # Ruby way #2
        pows = [128, 64, 32, 16, 8, 4, 2, 1]
        byte_val_ruby_2 = byte_chunk.zip(pows).sum { |a, b| a * b }
        _aa = 12

        # Ruby way #3
        pows = [128, 64, 32, 16, 8, 4, 2, 1]
        byte_val_ruby_3 = 0
        byte_chunk.each_with_index { |bit_int, idx| byte_val_ruby_3 += pows[idx] if bit_int == 1 }

        # C style
        # OUTCOME: I tried to come up with a "C style" implementation of this using bitwise
        # operators but my implementations always boiled down to doing
        # powers of 2 multiplication in a needlessly complicated way

        # quick assertion to check our work
        fail "ruby 1 to 2 mismatch" unless byte_val_ruby_1 == byte_val_ruby_2 && byte_val_ruby_1 == byte_val_ruby_3

        output << byte_val_ruby_1
      end

      [output, bit_ints.length]
    end
  end
end

def main
  input_buffer = "thisisatest"
  expected = [0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1,
              0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1,
              0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0]

  puts "-" * 80
  puts "Input bytes visualised:"
  p(input_buffer.bytes.map { _1.ord.to_s(2).rjust(8, "0") })
  puts "-" * 80

  p actual_1 = extract_bits_to_array_of_ints_1(input_buffer)
  puts "-" * 80
  p actual_2 = extract_bits_to_array_of_ints_2(input_buffer)
  puts "-" * 80

  fail "mismatch 1" unless actual_1 == expected
  fail "mismatch 2" unless actual_2 == expected

  puts "=" * 80
  puts "=" * 80
  # given a bit stream as Array<Integer>, write each Integer as a bit into a buffer
  input_array_of_bits = [0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1,
                         0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0,
                         0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0]

  p write_array_of_ints(input_array_of_bits)

  # inputs
  # ######

  # uneven multiple of 8 bits
  in_raw = "1011111011010100101101010010110010100101100001100101110001101111011110100001100010001111100011100011001110001000010001110101101111111000110001011011100011111000001111011101001111011101001111000011110001010"

  puts "in_raw:"
  puts in_raw

  puts "Hex version of in_raw:"
  puts in_raw.to_i(2).to_s(16).upcase

  # convert
  # #######

  len, out = binary_chars_to_bits(in_raw)
  File.write("temp.bin", out, encoding: Encoding::BINARY)
  File.write("temp.len", len, encoding: Encoding::BINARY)

  # read back
  # #########

  read_back_raw = File.read("temp.bin", encoding: Encoding::BINARY)
  read_back_len = File.read("temp.len", encoding: Encoding::BINARY)
  read_back = bits_to_binary_chars(read_back_raw, read_back_len)

  # verify
  # ######

  puts in_raw
  puts read_back
  fail "mismatch of input and read back" unless read_back == in_raw
end

main
