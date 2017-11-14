require "ripper"
require "pp"

code = 'puts "Hello, World!"'

puts code
# puts "Hello, World!"

pp Ripper.tokenize(code)
# ["puts", " ", "\"", "Hello, World!", "\"", "\n"]

pp Ripper.lex(code)
# [[[1, 0], :on_ident, "puts"],
#  [[1, 4], :on_sp, " "],
#  [[1, 5], :on_tstring_beg, "\""],
#  [[1, 6], :on_tstring_content, "Hello, World!"],
#  [[1, 19], :on_tstring_end, "\""],
#  [[1, 20], :on_nl, "\n"]]

pp Ripper.sexp(code) # output ruby AST
# [:program,
#  [[:command,
#    [:@ident, "puts", [1, 0]],
#    [:args_add_block,
#     [[:string_literal,
#       [:string_content, [:@tstring_content, "Hello, World!", [1, 6]]]]],
#     false]]]]

puts RubyVM::InstructionSequence.compile(code).disasm
# == disasm: #<ISeq:<compiled>@<compiled>>================================
# 0000 trace            1                                               (   1)
# 0002 putself
# 0003 putstring        "Hello, World!"
# 0005 opt_send_without_block <callinfo!mid:puts, argc:1, FCALL|ARGS_SIMPLE>, <callcache>
# 0008 leave
