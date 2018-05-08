
Thread.current == Thread.main

# how does this work?
# it must make the main thread wait for the value
puts Thread.new { 2 + 2 }.value


# calls to Thread.new return immediately
# call to Thread.new { ... }.value waits
