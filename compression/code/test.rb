require "json"

require_relative "./huffy/tree_node"
require_relative "./huffy/diagram"
require_relative "./huffy/encoding_map_generator"
require_relative "./huffy/priority_queue"
require_relative "./huffy/decoder"
require_relative "./huffy/tree_builder"
require_relative "./huffy/encoder"

require "bindata"

module Huffman
  class Packager
    def pack(encoder_output, length)
    end

    def unpack
    end
  end
end

class HuffmanBlob < BinData::Record
  endian :big

  uint64 :bits_len
  bit :data, nbits: :bits_len
end

class Main
  def self.main
    input_str = File.read(File.join(__dir__, "input.txt"))

    encoder = Huffman::Encoder.new
    encoder.encode(input_str)

    # Write some output files to help debugging
    File.write(File.join(__dir__, "output.txt"), encoder.output)
    File.write(File.join(__dir__, "output_huffman_tree_diagram.md"), encoder.mermaid_diagram)
    File.write(File.join(__dir__, "output_decoding_map.json"), JSON.pretty_generate(encoder.decoding_map))

    # create the bindata instance
    _blob = HuffmanBlob.new(data_length: encoder.output_length, data: encoder.output)

    # File.write(File.join(__dir__, "output_packed.txt"), encoder.packed_output)

    decoder = Huffman::Decoder.new(encoder.decoding_map)
    decoded_input_str = decoder.decode(encoder.output)

    fail "Mismatch" unless input_str == decoded_input_str
  end
end

Main.main
