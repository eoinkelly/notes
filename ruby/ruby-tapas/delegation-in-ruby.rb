require 'pry'
require 'delegate' # load delegate library

##
#
class Foo
  def woof
  end

  def blah
  end
end

##
#
class AugmentedFoo < SimpleDelegator
  def initialize(foo)
    super(foo) # calls the initialize for Foo
  end

  def woof
    puts 'special'
    super # call foo#woof
  end
end

##
#
class AugmentedFoo2 < DelegateClass(Foo)
  def initialize(foo)
    super(foo) # calls the initialize for Foo
  end

  def woof
    puts 'special'
    super # call foo#woof
  end
end

binding.pry
puts 'done'


