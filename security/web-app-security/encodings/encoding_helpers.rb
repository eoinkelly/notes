class EncodingHelper
  class << self
    # convert all characters in a UTF-8 string to %uXXXX encoded
    # assumes input is UTF-8 string
    def create_unicode_encoded_rep(input)
      input                                                                         # => "aé…"
        .unpack("U*")                                                               # => [97, 233, 8230]
        .map { |codepoint_as_decimal| format("%04X", codepoint_as_decimal) }        # => ["0061", "00E9", "2026"]
        .map { |codepoint_as_zero_padded_hex| "%u#{codepoint_as_zero_padded_hex}" } # => ["%u0061", "%u00E9", "%u2026"]
        .join                                                                       # => "%u0061%u00E9%u2026"
    end

    # assumes input is UTF-8 string
    def percent_encode_all_chars(input)
      input                                                        # => "aé…"
        .unpack("U*")                                              # => [97, 233, 8230]
        # convert from dec to hex
        # split into bytes (hex pairs) and prefix with %
        # .map { |codepoint_as_decimal| format("%04X", codepoint_as_decimal) }        # => ["0061", "00E9", "2026"]
        # .map { |codepoint_as_zero_padded_hex| "%u#{codepoint_as_zero_padded_hex}" } # => ["%u0061", "%u00E9", "%u2026"]
        # .join                                                                       # => "%u0061%u00E9%u2026"
    end
  end
end
