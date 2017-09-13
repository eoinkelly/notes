require "ripper"
require "pp"

code = <<-EOCODE
10.times do |n|
  puts n
end
EOCODE

puts code
# 10.times do |n|
#   puts n
# end

puts RubyVM::InstructionSequence.compile(code).disasm
# == disasm: #<ISeq:<compiled>@<compiled>>================================
# == catch table
# | catch type: break  st: 0002 ed: 0008 sp: 0000 cont: 0008
# |------------------------------------------------------------------------
# 0000 trace            1                                               (   1)
# 0002 putobject        10
# 0004 send             <callinfo!mid:times, argc:0>, <callcache>, block in <compiled>
# 0008 leave
# == disasm: #<ISeq:block in <compiled>@<compiled>>=======================
# == catch table
# | catch type: redo   st: 0002 ed: 0010 sp: 0000 cont: 0002
# | catch type: next   st: 0002 ed: 0010 sp: 0000 cont: 0010
# |------------------------------------------------------------------------
# local table (size: 1, argc: 1 [opts: 0, rest: -1, post: 0, block: -1, kw: -1@-1, kwrest: -1])
# [ 1] n<Arg>
# 0000 trace            256                                             (   1)
# 0002 trace            1                                               (   2)
# 0004 putself
# 0005 getlocal_OP__WC__0 3
# 0007 opt_send_without_block <callinfo!mid:puts, argc:1, FCALL|ARGS_SIMPLE>, <callcache>
# 0010 trace            512                                             (   3)
# 0012 leave                                                            (   2)

pp Ripper.tokenize(code)
# ["10",
#  ".",
#  "times",
#  " ",
#  "do",
#  " ",
#  "|",
#  "n",
#  "|",
#  "\n",
#  "  ",
#  "puts",
#  " ",
#  "n",
#  "\n",
#  "end",
#  "\n"]

pp Ripper.lex(code)
# [[[1, 0], :on_int, "10"],
#  [[1, 2], :on_period, "."],
#  [[1, 3], :on_ident, "times"],
#  [[1, 8], :on_sp, " "],
#  [[1, 9], :on_kw, "do"],
#  [[1, 11], :on_sp, " "],
#  [[1, 12], :on_op, "|"],
#  [[1, 13], :on_ident, "n"],
#  [[1, 14], :on_op, "|"],
#  [[1, 15], :on_ignored_nl, "\n"],
#  [[2, 0], :on_sp, "  "],
#  [[2, 2], :on_ident, "puts"],
#  [[2, 6], :on_sp, " "],
#  [[2, 7], :on_ident, "n"],
#  [[2, 8], :on_nl, "\n"],
#  [[3, 0], :on_kw, "end"],
#  [[3, 3], :on_nl, "\n"]]


pp Ripper.sexp(code) # output ruby AST
# [:program,
#  [[:method_add_block,
#    [:call, [:@int, "10", [1, 0]], :".", [:@ident, "times", [1, 3]]],
#    [:do_block,
#     [:block_var,
#      [:params, [[:@ident, "n", [1, 13]]], nil, nil, nil, nil, nil, nil],
#      false],
#     [[:command,
#       [:@ident, "puts", [2, 2]],
#       [:args_add_block, [[:var_ref, [:@ident, "n", [2, 7]]]], false]]]]]]]

