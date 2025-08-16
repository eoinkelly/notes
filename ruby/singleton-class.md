class Cow def say puts "hi" end end

morty = Cow.new

# this:

def morty.greet puts "greetings from morty" end

# is shorthand for:

class << morty

# the `class << morty` is ruby syntax that opens up the singleton class for

# the `morty` object

def greet puts "greetings from morty" end end

# or this:

morty.singleton_class.class_exec do def greet puts "greetings from morty" end
end

Cow.class # => Class Cow.class.methods(false) # => []

Cow.singleton_class # => #<Class:Cow> Cow.singleton_class.methods(false) # => []

Cow.methods(false) # => [] Cow.instance_methods(false) # => [:say]

morty.singleton_class # => #<Class:#<Cow:0x007fd1494ab240>>
morty.singleton_class.methods(false) # => []
morty.singleton_class.instance_methods(false) # => [:greet]

class Foo # this syntax opens up the singleton class and dates from before #
ruby had #singleton_class methods class << self end end

class Foo end

# class methods in ruby are actually methods defined on the singleton

# class of the class object

class Foo # 'self' within a class refers to the class itself. in the same way
that # defining a method directly on an object will actually define the method #
on the objects singleton class, here the "object" is the class object so # that
is waht we are doing def self.hello "hello" end end

Foo.singleton_class.class_exec do def self.hello "hello" end end

# an explicit way of defining a class method

class Foo self.singleton_class.class_exec do def hello "hello" end end end
