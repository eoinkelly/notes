module Huffman
  class TreeNode
    attr_accessor :left, :right, :symbol, :weight

    def initialize(left: nil, right: nil, symbol: nil, weight: nil)
      @left = left
      @right = right
      @symbol = symbol
      @weight = weight
    end

    # FYI not actually using this
    # def to_h
    #   output = { weight: @weight }
    #   output[:symbol] = @symbol if @symbol
    #   output[:left] = left.to_h if @left
    #   output[:right] = right.to_h if @right
    #   output
    # end

    def inspect
      "<#{self.class.name}:#{object_id}: internal: #{internal_node?}, #{if @symbol
                                                                          "symbol: #{@symbol}, "
                                                                        end}weight: #{@weight}>"
    end

    def leaf_node?
      @left.nil? && @right.nil?
    end

    def internal_node?
      !leaf_node?
    end
  end
end
