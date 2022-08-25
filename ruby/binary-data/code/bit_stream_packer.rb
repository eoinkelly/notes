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
        if actual_chunk_len < REQUIRED_CHUNK_LEN
          num_padding_bits = REQUIRED_CHUNK_LEN - actual_chunk_len
          byte_chunk += Array.new(num_padding_bits, 0)
        end

        buffer << byte_chunk.map(&:to_s).join.to_i(2)
      end

      [buffer, input_stream.length]
    end

    def unpack(buffer, len)
    end
  end
end

def test
  input_buffer = [
    0, 1, 1, 1, 0, 1, 0, 0, # t
    0, 1, 1, 0, 1, 0, 0, 0, # h
    0, 1, 1, 0, 1, 0, 0, 1, # i
    0, 1, 1, 1, 0, 0, 1, 1, # s
    0, 1, 1, 0, 1, 0, 0, 1, # i
    0, 1, 1, 1, 0, 0, 1, 1, # s
    0, 1, 1, 0, 0, 0, 0, 1, # a
    0, 1, 1, 1, 0 # 0, 0, 0 # p
  ]
  expected_packed = "thisisap"
  exptected_len = input_buffer.length

  packed, len = BitStreamPacker.pack(input_buffer)
  puts "-" * 80
  pp len
  pp packed
  puts "-" * 80

  fail "mismatch" unless packed == expected_packed && len == exptected_len

  unpacked = BitStreamPacker.unpack(buffer, len)
  puts "-" * 80
  pp unpacked
  puts "-" * 80
  # fail "mismatch" unless packed == expected_packed && len == exptected_len
end

test
