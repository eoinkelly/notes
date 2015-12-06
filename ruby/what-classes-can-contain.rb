require 'pry'

# http://www.devalot.com/articles/2008/09/ruby-singleton

# ruby only supposts instance methods - it does not support class methods
# * ruby classes are actually objects.
# * Their names are constants that point to the object instantiated from the Class class
# * The object that the constant points to holds all the intance methods for objects initialized from it
# * so-called "class methods" are kept on a singleton class


class Thing
  def initialize  # always a private method
    # this gets called whenever this class is manufacturing a new instance
  end

  @@class_var = 23 # affected by public/private/protected?

  # Constants ##########################
  MyCons1 = 33
  MY_CONSTANT = 44

  MySecretConstant = 66
  private_constant :MySecretConstant # new in 1.9.3, this is the only way to make class constants private.

  # Instance Variables ##########################
  @instance_var = 55
  # does it make any sense to have this outside initialize?
  # declared outside initialize so it comes into existance as soon as the class block is read by the interpreter

  # Class Level Methods ##########################

  # * By default all class methods are public and can be accessed by
  #   * Thing::i_am_public (anywhere in code)

  #   * i_am_public (anywhere within Thing defn. block, anywhere within the
  #   * class defn. of anything inheriting from Thing, any instance of Thing,
  #   * within any instance of a class that inherits from Thing)

  # actually creates a method in a singleton class
  #
  def self.i_am_public
    "by default"
  end

  self.singleton_methods # => [:i_am_public]
  self.class # Class
  self # Thing

  def self.am_i_private?
    "no"
  end

  # private :am_i_private? # Fails with NameError as it can't find the method name
  private_class_method :am_i_private? # works

  # * class methods do operations on this type that don't pertain to a particular instance of it

  public

  def a
  end

  protected

  def b
  end


  private


  def c
  end

end

binding.pry