require 'delegate'
require 'date'

class User
  def birth_date
    puts "user #birth_date"
    Date.new(1979,03,22)
  end
end

class UserDecorator < SimpleDelegator
  def birth_date
    puts "decorator #birth_date"
    # we can call super from any method in this object and it will be routed to
    # the corresponding method in the underlying class
    super 
  end

  def birth_year
    puts "decorator #birth_year"
    birth_date.year
  end
end

decorated_user = UserDecorator.new(User.new)

p decorated_user.birth_year
p decorated_user.__getobj__
p decorated_user.class


require 'forwardable'

# Forwardable allows you to delegate named methods from one class to another
# * Unlike SimpleDelgator you have to explicitly say what you want to delegate
# * You don't have to forward all calls to the same object (unlike SimpleDelegator)
# * You can use Forwardable to just inherit part of an interface (inheritance forces you get the whole interface)
# * Forwardable is much more granular than SimpleDelegator
class Bar
  extend Forwardable
end
