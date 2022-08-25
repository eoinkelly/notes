module Huffman
  class TreeBuilder
    def initialize(input_symbols)
      @input_symbols = input_symbols
    end

    def build_tree
      weights_map = @input_symbols.each_with_object(Hash.new(0)) { |symbol, acc| acc[symbol] += 1 }
      # input_weights = @input_symbols.map { |s| weights_map[s] }
      priority_queue = Huffman::PriorityQueue.new

      @input_symbols
        .uniq
        .map { |symbol| Huffman::TreeNode.new(symbol:, weight: weights_map[symbol]) }
        .each { |tree_node| priority_queue.push(tree_node, tree_node.weight) }

      while priority_queue.length > 1
        left, right = priority_queue.pop(2)
        new_node = Huffman::TreeNode.new(left:, right:, weight: (left.weight + right.weight))
        priority_queue.push(new_node, new_node.weight)
      end

      # queue should only have one node left which is our root node
      priority_queue.pop(1).first
    end
  end
end
