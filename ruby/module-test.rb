require 'pry'

module Alpha

  PI = 3.14
  @dunno = 23 # out here self will always point at the Alpha module object

  def foo
    # @vary will clash iwth any @vary instance variable in the class we are  included into - BE CAREFUL
    @vary = 12 # in here self will point at the class object that includes us
    puts "i am Alpha.foo"
  end

  def initialize
    # a module can have an initialize call but this will only be invoked if
    # 1. the class does not have it's own initiailze method
    # 2. the class's initialize method calls super
end

module Beta

  PI = 3.15

  def foo
    puts "i am Beta.foo"
  end
end

class Eoin
  # * adds the methods in Alpha to the singleton superclass of Eoin
  include Alpha

  # in here self = Eoin so calling extend is equivalent ot self.extend which is
  # Eoin.extend this means that calling extend within a class definition will
  # add the methods of Beta to the anonymous singleton superclass of Eoin
  # this means the methods in Beta will be class level methods of Eoin
  extend Beta
end

e = Eoin.new
e.foo

binding.pry
# Alpha.foo
# Beta.foo