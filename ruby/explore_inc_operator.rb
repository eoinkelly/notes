# ruby has no ++ and -- operators but
# 23 -- 4 # => 27
# wtf?
require 'ripper'
require 'pp'

code = <<STR
23 -- 4
STR

puts '%%%%%%%%%%%%%%%%%%%%%%%%%'
puts code
puts '%%%%%%%%%%%%%%%%%%%%%%%%%'
pp Ripper.lex(code)
puts '%%%%%%%%%%%%%%%%%%%%%%%%%'
pp Ripper.sexp(code)
puts '%%%%%%%%%%%%%%%%%%%%%%%%%'

# Answer: ruby interprets this as
# 23 - -4
# also
# 23 ++ 4
# is interpreted as
# 23 + +4
