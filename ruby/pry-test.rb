#!/usr/bin/ruby -w

# -w turns warnings on for ruby just like perl

require 'pry'

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


t = Thing.new('foo')
x = 10

binding.pry

puts "I am after pry session. x is now #{x}"