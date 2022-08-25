module Huffman
  ##
  # Represent the given binary tree `Huffman::TreeNode` as a mermaid diagram
  #
  class Diagram
    NODE_NAME_PREFIX = "node".freeze # Mermaid nodes cannot begin with a number

    def initialize(tree_node)
      @tree_node = tree_node
      @lines = []
    end

    def as_mermaid_diagram
      visit_node(@tree_node)

      <<~EO_MERMAID
        ```mermaid
        graph TD
        #{@lines.join("\n")}
        ```
      EO_MERMAID
    end

    def visit_node(node)
      @lines << if node.leaf_node?
                  %Q{  #{NODE_NAME_PREFIX}#{node.object_id}((("'#{node.symbol}' #{node.weight}")))}
                else
                  %Q{  #{NODE_NAME_PREFIX}#{node.object_id}(("#{node.weight}"))}
                end

      if node.left
        @lines << "  #{NODE_NAME_PREFIX}#{node.object_id} -->|0| #{NODE_NAME_PREFIX}#{node.left.object_id}"
        visit_node(node.left)
      end

      if node.right # rubocop:disable Style/GuardClause
        @lines << "  #{NODE_NAME_PREFIX}#{node.object_id} -->|1| #{NODE_NAME_PREFIX}#{node.right.object_id}"
        visit_node(node.right)
      end
    end
  end
end
