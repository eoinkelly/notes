require "pry"

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
