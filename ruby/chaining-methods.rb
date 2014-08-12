
# Attempt 1
# #########

def foo(x)
  x + 3
end

def bar(y)
  y + 5
end

p foo(12).bar(15)
# * This won't work becaue foo(12) returns an int and we can't send the bar
#   message to an int

# Attempt 2
# #########

class Thing
  def foo(x)
    x + 3
  end

  def bar(y)
    y + 5
  end
end

t = Thing.new
p result = t.foo(12).bar(15)

# * This won't work becaue foo(12) returns an int and we can't send the bar
#   message to an int.
# * To fix this we need to have foo return self but that means that foo cannot
#   also return the new value.

# Attempt 3
# #########

class Thing
  def initialize
    @val = 0
  end

  def foo(x)
    @val += x
    self
  end

  def bar(y)
    @val += y
    self
  end
end

t = Thing.new
p result = t.foo(12).bar(15) # works
