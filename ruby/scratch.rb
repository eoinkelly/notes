require 'pry'

# main (top level object instance)
# ###############################

# ruby programs always startin in "main", the top-level object instance
# main is an instance of Object with the special property that any methodes defind there are added as instance methods of Object (so they are available everywhere)
# the 'main' object is special and has a dual Class/Object hybrid nature - good article on it: http://banisterfiend.wordpress.com/2010/11/23/what-is-the-ruby-top-level/







# The "sender" in ruby is the owner of the scope that the message originated in.

# * You can refer to it in pry with self.<message-name>

# [1] pry(main)> ls
# self.methods: include  private  public  to_s
# locals: _  _dir_  _ex_  _file_  _in_  _out_  _pry_

# * Classes in ruby are first-class objects
# * When a class is created in ruby (typically using class Foo ... end) an object of type Class is created and assigned to a global constant 'Foo'

# In ruby docs ::foo indicates that foo is a class method (i.e. a method on the class object) and #foo indicates that foo is an method of the instance of the class
#? are class methods available in instances?

# Class::new basicaly urns Class#allocate and then invokes the object's initialize method passing on all args
# Class#superclass => returns the
# Override the class#new method

# .superclass retuns the actual class object, not it's name or anything else
# Class.superclass # => Module
# Module.superclass # => Object
# Object.superclass # => BasicObject
# BasicObject.superclass # => nil
# Class.superclass.superclass.superclass.superclass # => nil

# class Class
#   alias oldNew new

#   def new(*args)
#     print "Creating a new ", self.name, "\n"
#     oldNew(*args)
#   end
# end


class Person

  # class variables
  # ###############

  # A class variable is shared by all instances created by the class in the system
  # It is "global to all Person objects"
  # Any one of those object can read & write it
  # Any of the class methods can also access it

  # ruby will create 1 instance of a Class object and assign it to the constant 'Person'
  # class variables within that object are shared with all objects that are created by it
  # so a class variable is shared between the "creator" Class object and all the objects created by it
  @@someyoke = "hey there" # create a class variable, shared between this

  # You can't do "class-level instance variables" in ruby without some f**king around.

  # class level constants
  # #####################

  MaxThings = 30 # anythign that begins with an uppercase letter is treated as a constant
  MAX_OTHER_STUFF = 40 # this is a constant (only because it begins with an uppercase letter)

  # outside Person use:
  # puts Person::MaxThings
  # Person::MaxThings = 66 # works but generates a warning, constants are not enforced by Ruby!

  # within Person
  def self.get_the_max
    MaxThings
  end

  # * Available everywhere


  # define a class method (by prefixing the method name iwth the name of the class)
  # I guess it's kind of putting an explicit receiver on the method.
  # You can access this

  # ruby and self
  # #############

  # In ruby you are always sending a message to some object. If you don't provide an explicit receiver, ruby uses self
  # * self does change depending on where in the code it is used
  # There are only 2 things that change what self points to:
  # 1. You specify a receiver explicitly e.g. Person.dosomething
  # 2. You are within a class or module definition block (self points to the Class object in here)

  def Person.dosomething
    puts "doing a thing"
    @@someyoke << ", Person.dosomething()" # just proving that the class variable can be changed in here
  end

  # Note attr_accessor sets up *instance* variables, not class variables
  attr_accessor :name, :age, :height, :weight

  # here I am just declaring some public methods in the Person object (which is a factory
  # for creating persons). These methods know how to create instance variables
  # when they are called i.e. @name does not exist until some code calls name=

  # The Person object (a class object) responds to the following messages:
  # new
  # age
  # age=
  # name
  # name=
  # height
  # height=
  # weight
  # weight=

  private

  def Person.do_private
    puts "doing a private thing"
  end

end

class IrishPerson < Person
  attr_accessor :county, :parish
end

eoin = IrishPerson.new
eoin.name = "Oi"
eoin.age = 33
eoin.county = "LImerick"

# puts "Starting Pry Session"
# binding.pry
# puts "Finished Pry Session"

# Introspecting a Ruby Object
# ###########################

# foo is an instance of Foo

# foo.class # returns Foo
# foo.superclass # ERROR, only Class objects have a #superclass method, objects created by class objects do not
# foo.superclass # returns the class object that is our superclass
# foo.methods # get an array of all the public?private messages that this class will respond to (all array functions work on the returned value)
# foo.methods - Object.methods # returns an array of the methods that foo responds to that are not in the base object
# foo.object_id # return the unique object ID
# foo.instance_variables # returns an array of strings of instance variable names
# foo.class.class_variables
# Foo.name # only exists on Class objects
# foo.kindof?(Module) # is foo a module
# foo.public_methods
# foo.class.instance_methods
# method(:method_name_as_symbol).owner # who owns this method
# foo.class
# foo.respondto? "to_s" # will this object respond ot the "to_s" message
# foo.singleton_methods # what singleton methods are defined in foo's anonymous (singleton) parent