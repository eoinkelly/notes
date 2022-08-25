module Huffman
  class PriorityQueue
    def initialize
      @values = []
      @sorted = true
    end

    ##
    # Push a value onto the queue with a priority.
    #
    # Lower `priority` numbers are higher priority
    #
    def push(value, priority)
      @values << { value:, priority: }
      @sorted = false

      nil
    end

    ##
    # Get the n **highest** priority items from the queue
    # @return [Array<Huffman::TreeNode>]
    #
    def pop(n = 1) # rubocop:disable Naming/MethodParameterName
      sort_values unless sorted?
      @values.shift(n).map { |val| val.fetch(:value) }
    end

    def length
      @values.length
    end

    private

    def sorted?
      @sorted
    end

    def sort_values
      @values.sort! { |a, b| a.fetch(:priority) <=> b.fetch(:priority) }
      @sorted = true
    end
  end
end
