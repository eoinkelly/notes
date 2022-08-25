module Huffman
  class Decoder
    def initialize(encodings_map)
      @map = encodings_map
    end

    def decode(input)
      output = ""
      acc = ""

      # read symbols from input until we get something which matches a mapping, then
      # copy that symbol into the output, reset the accumulator and start again
      input.chars.each do |symbol|
        acc << symbol

        if @map[acc]
          output << @map[acc]
          acc = ""
        end
      end

      output
    end
  end
end
