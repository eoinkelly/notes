# load this file in IRB or pry to play with ruby autoload
# autoload :Thing, "thing"
class Thing
  def initialize(some_var)
    @some_var = some_var
  end

  def foo
    puts "hello"
  end

  def boo
    puts "blah"
  end
end
