require "pry"
require "pry-state"

class Foo1
  def initialize(name)
    @name = name
  end
end
f1 = Foo1.new("Eoin")

# or

Foo2 = Class.new
f2 = Foo2.new
f2.instance_variable_set(:@name, "Eoin")

p f1
p f2

class Module
  def const_missing(c)
    puts "You asked for uknown const #{c}"
    super
  end
end

# Always implement #respond_to_missing? when you implement #method_missing!!!
class SomeWrapper
  def method_missing(name, *args, &blk)
    # filter out the method names you care about and do stuff
    # for everything else call `super`
    super # the "zsuper"
  end

  def respond_to_missing?(name, private = false)
  end
end

binding.pry

puts "done"
