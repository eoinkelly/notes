require 'pry'

class Some
  def initialize(object)
    @object = object
  end

  def map # takes a block

    # * This says nothing about whether we mutate our @object or not
    # * it is the _return value_ of the block that is used
    # * if your block happens to mutate @object then nothing will stop it
    new_object = yield @object # new_object is the _return value_ of the block

    # and now wrap it in a Some
    Some.new new_object

    # or the short version ...
    # Some.new yield(@object)
  end

  def unwrap_or(default)
    @object
  end
end

class None
  def map
    self
  end

  def unwrap_or(default)
    default
  end
end


class Person
  attr_accessor :age, :name

  def initialize(name, age)
    @name = name
    @age = age
  end
end

eoin = Person.new("Eoin Kelly", 35)
maybe_person = Some.new(eoin)

x = maybe_person.map {|p| "{#{p.age} is great" }

binding.pry
