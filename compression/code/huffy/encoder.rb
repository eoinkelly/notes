module Huffman
  class Encoder
    attr_reader :output, :encoding_map, :decoding_map, :mermaid_diagram, :output_length

    def initialize
      @output = nil
      @output_length = 0
      @encoding_map = nil
      @decoding_map = nil
      @mermaid_diagram = nil
    end

    def encode(input_str)
      input_symbols = input_str.chars

      # TODO: a lot of this could be lazier, moved into readers etc.
      huffman_tree = Huffman::TreeBuilder.new(input_symbols).build_tree
      map_generator = Huffman::EncodingMapGenerator.new(huffman_tree)

      @encoding_map = map_generator.generate_encoding_map
      @decoding_map = map_generator.generate_decoding_map
      @mermaid_diagram = Huffman::Diagram.new(huffman_tree).as_mermaid_diagram

      encoded_symbols = input_symbols.map { |sym| @encoding_map[sym] }

      @output = encoded_symbols.join
      @output_length = encoded_symbols.length

      nil
    end
  end
end
