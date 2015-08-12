
def foo
  puts 'in foo'
  bar
end

def bar
  puts 'in bar'
  baz
end

def baz
  puts 'in baz'
  # puts caller(0) # tell #caller to include current frame
  # puts caller # does not show current frame (baz) by default
end

tracer = TracePoint.new() do |tp|
  next if tp.event == :line
  p tp
end

tracer.enable do
  foo
end
