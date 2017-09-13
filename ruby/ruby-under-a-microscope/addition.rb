require "ripper"
require "pp"

code = <<-EOCODE
puts 2 + 2
EOCODE

def divider
  puts "======================================"
end

puts code
divider
pp Ripper.tokenize(code)
divider
pp Ripper.lex(code)
divider
pp Ripper.sexp(code)
divider
puts RubyVM::InstructionSequence.compile(code).disasm
