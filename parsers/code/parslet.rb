require 'parslet'

class Mini < Parslet::Parser

  # tip: have every atom consume any whitespace after it because it makes the
  # top level rules look cleaner since they don't have all the whitespace stuff
  # stuck in a few top level rules

  # match any char in 0-9 but at least once
  rule(:integer) { match('[0-9]').repeat(1) >> space? }

  rule(:space) { match('\s').repeat(1) }
  rule(:space?) { space.maybe } # space.repeat(0, 1)

  rule(:operator) { match('[+]') >> space? }

  rule(:sum) { integer >> operator >> expression }
  rule(:expression) { sum | integer }

  # stat parsing at the given  rule
  root(:expression)
end

def parse(str)
  Mini.new.parse(str)
rescue Parslet::ParseFailed => e
  puts e.parse_failure_cause.ascii_tree
end

p parse("132432")  # => "132432"@0
p parse("1 + 4")  # => "132432"@0
p parse("a + 4")  # => "132432"@0
# p parse("puts(4 + 3)")  # => "132432"@0
