require "ripper"
require "pp"

code = 'puts "Hello, World!"'
actual = Ripper.sexp(code)
# [:program,
#  [[:command,
#    [:@ident, "puts", [1, 0]],
#    [:args_add_block,
#     [[:string_literal,
#       [:string_content, [:@tstring_content, "Hello, World!", [1, 6]]]]],
#     false]]]]

# Shape of a node:
# [:identifier, c1, c2, c3, ..., cn]

n_str_content = [:@tstring_content, "Hello, World!", [1, 6]]
n_content = [:string_content, n_str_content]
n_literal = [:string_literal, n_content]
n_args_add_block = [:args_add_block, [n_literal], false]
n_puts_ident = [:@ident, "puts", [1, 0]]
n_command = [:command, n_puts_ident, n_args_add_block]
n_program = [:program, [n_command]]

if actual == n_program
  puts "output identical"
else
  puts "problem"
end
pp actual
pp n_program


"👩‍👩‍👧‍👧".codepoints
# => [128105, 8205, 128105, 8205, 128103, 8205, 128103]
"👩‍👩‍👧‍👧".chars
# => ["👩", "", "👩", "", "👧", "", "👧"]



# receiver.message_name(arg_1, arg_2)
puts "Hello World!"
self.puts "Hello World!"
puts self       # => "main"
puts self.class # => "Object"