require "ripper"
require "pp"

code = <<-EOCODE
puts "hello, world!"
EOCODE

puts code
pp Ripper.lex(code)

# puts "hello, world!"
# [[[1, 0], :on_ident, "puts"],
#  [[1, 4], :on_sp, " "],
#  [[1, 5], :on_tstring_beg, "\""],
#  [[1, 6], :on_tstring_content, "hello, world!"],
#  [[1, 19], :on_tstring_end, "\""],
#  [[1, 20], :on_nl, "\n"]]
