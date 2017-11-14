require "ripper"
require "pp"

code = 'puts "Hello, World!"'
pp Ripper.tokenize(code)
# ["puts", " ", "\"", "Hello, World!", "\"", "\n"]

require "ripper"
require "pp"

code = 'puts "Hello, World!"'
pp Ripper.lex(code)
# [[[1, 0], :on_ident, "puts"],
#  [[1, 4], :on_sp, " "],
#  [[1, 5], :on_tstring_beg, "\""],
#  [[1, 6], :on_tstring_content, "Hello, World!"],
#  [[1, 19], :on_tstring_end, "\""],
#  [[1, 20], :on_nl, "\n"]]

require "ripper"
require "pp"

code = 'puts "Hello, World!"'
pp Ripper.sexp(code)
# [:program,
#  [[:command,
#    [:@ident, "puts", [1, 0]],
#    [:args_add_block,
#     [[:string_literal,
#       [:string_content, [:@tstring_content, "Hello, World!", [1, 6]]]]],
#     false]]]]

code = 'puts "Hello, World!"'
puts RubyVM::InstructionSequence.compile(code).disasm
# == disasm: #<ISeq:<compiled>@<compiled>>===========
# 0000 trace            1                           (   1)
# 0002 putself
# 0003 putstring        "Hello, World!"
# 0005 opt_send_without_block <callinfo!mid:puts, argc:1, FCALL|ARGS_SIMPLE>, <callcache>
# 0008 leave




# Shape of a node:
# [:identifier, c1, c2, c3, ..., cn]


n_content = [:@tstring_content, "Hello, World!", [1, 6]]
n_literal = [:string_literal, n_content]
n_args_add_block = [:args_add_block, n_literal, false]
n_puts_ident = [:@ident, "puts", [1, 0]],
n_command = [:command, n_puts_ident, n_args_add_block]
n_program = [:program, n_command]
pp n_program