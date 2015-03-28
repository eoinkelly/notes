# Fixture data
def doubler(x)
  x * 2
end

class Stuff
  def doubler(x)
    x * 2
  end
end

class Fixnum
  def monkey_doubler
    self * 2
  end
end

aa = [1, 2, 3, 4, 5]

p aa.map(&:monkey_doubler) # works because Fixnum#monkey_doubler exists

def mk_proc
  proc { |x| x * 2 }
end

# & in a method signature converts the passed block to a Proc instance
#
# & in any other place will call #to_proc on whatever is after it in an attempt
# to coerce it into a block
#
p aa.map(&mk_proc)

# Object#method returns an instance of Method for the method with the same name
# as the symbol. Instances of Method have a #to_proc so this works
#
p aa.map(&method(:doubler))

# this demos pulling a method object from an arbitrary class
m = Stuff.new.method(:doubler)
p aa.map(&m)
