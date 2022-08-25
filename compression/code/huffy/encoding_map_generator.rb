module Huffman
  class EncodingMapGenerator
    ZERO = "0".freeze
    ONE = "1".freeze

    def initialize(huffman_tree)
      @huffman_tree = huffman_tree
      @encoding_map = {}
    end

    def generate_encoding_map
      visit_node(@huffman_tree, path: [""])
      @encoding_map
    end

    def generate_decoding_map
      generate_encoding_map if @encoding_map == {}
      @encoding_map.invert
    end

    def visit_node(node, path:)
      # puts "visiting #{node.inspect}, path=#{path.inspect}"
      if node.leaf_node?
        # emit a code based on the queue of paths passed in
        @encoding_map[node.symbol] = path.join
        return
      end

      visit_node(node.left, path: path.clone.push(ZERO)) if node.left
      visit_node(node.right, path: path.clone.push(ONE)) if node.right
    end
  end
end
