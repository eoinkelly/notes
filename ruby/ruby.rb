# Inbox
# https://peepcode.com/screencasts/ruby
# http://tryruby.org/levels/7/challenges/5
# http://railsforzombies.org/levels/1
# http://objectsonrails.com/

# Nomenclature
# ############

# Since everything in ruby is an object
# method = function
# attachment = an object provided at invocation of a method and passed to it
# symbol = a string prefixed by :

# idiom = a phrase that has a figurative meaning that is understood by the
# common use as well as it's literal meaning e.g. "she is pulling my leg"



# Printing to stdout
# ##################

print "do i get returned?" # prints argument without newline, returns nil
puts "do i get returned?" # prints argument with newline, returns nil
p "string object" # prints a dump of the argument to STDOUT, returns the argument




# Operators
# #########

# operator are just syntactic sugar for methods. They are also called "symbolic" methods

# . is the "message passing" operator in ruby

# Logical Operators

# && and 'and' work the same except && has higher precedence http://phrogz.net/ProgrammingRuby/language.html#table%5F18.4
# || and 'or' work the same except && has higher precedence http://phrogz.net/ProgrammingRuby/language.html#table%5F18.4
# All of ||, and, or, && use "short circuit evaluation" (where the RHS operand is only evaluaged if the LHS one has a particular value)

# Objects
# #######

# *everything* is an object

# Chapter 6 - Standard Types
# ##########################

# The ruby basic types are
# hash, array, proc,
# number (Fixnum, Bignum, Float)
# string, range, regular expression

# Integers
# Ruby uses the Fixnum and Bignum classes to represent integers - does so trasparently
# Integers are <optional sign><optional base indicator><string of digits incl. underscores>
# Underscores are ignored in numbers - sometimes used to clarify big ones

# These are all represented by Fixnum & Bignum
my_binary_num = 0b001_001_000
my_hex_num = 0xffeeaa
my_octal_num = 0777
my_specified_decimial = 0d12345
huge_num = 123345554234
huge_num = 123_345_554_234 # underscores make large numbers easier to ready like commas in real life

# Watch out for integer literals with leading 0 - ruby will think they are octals
0567 # => 375
0589 # returns SyntaxError exception as 0589 is not a valid octal number

# Strings that contain just digits are not automatically converted to numbers by ruby
x = "123"
x * 2 # => "123123"
x.to_i * 2 # => 246
Integer(x) * 2 # => 246

# When does ruby turn a numberic literal into a Float?
# A numeric literal with a decimal point and/or an exponent will be converted to a Float object

# Gotcha: There must be a digit either side of the decimal point - otherwise ruby thinks its the message passing operator

1.0e3 # works
1.e3 # ruby tries to call e3 method on 1

# If you mix integers and floats the result will be a float

# Gotcha If you divide integers the result will be an integer!
22 / 7 # => returns 3 - ouch!
22.0 / 7 # => 3.142857 etc. because one number is a float, the result is a float

# Iterators
# Integers support a number of iterators:
4.times {|i|}
5.upto(10)
60.downto(40) {|i| p i }

# .step is provided by the Numeric class
50.step(80, 5) { |i| puts i } # => prints 50, 55, 60, 65, 70, 75, 80, returns 50
50.step(30, 5) { |i| puts i } # doesn't work

# The mathn library has lots of good stuff for dealing wiht numbers

# Strings
# #######

# ruby strings can hold printable characters or binary data (you'll need to use escape chars to put binary data into string literals)
# when ruby finds escape characters in a string literal, it *replaces* them with their binary equivalent before doing anythign with them.
# #{} does an implicit to_s so you don't need to

a = %w{hello there}

# both these output the same thing
puts "Message is:#{a.to_s}"
puts "Message is:#{a}" # implicit to_s

@var = "hello"
vary = "blah"

puts "Message is: #vary" # => "Message is #vary", ruby can't tell that this is a variable
puts "Message is:#@var" # If it's a global($), instance(@) or class (@@) variable ruby can tell what it is by variable prefix so you can omit the {}

puts "Answer is: #{
  # arbitrarily complex stuff can go in here
  def funky(a)
    'funk level' + a.to_s
  end
  funky(12)
}"

# 5 ways of creating string literals
'I am a single quoted string' #1: using single quotes, no variable interpolation or backslash replacement
"I am a double quoted string" #2 using double quotes, does variable interpolation and backslash replacement
%q/I'm a single quoted string/ #3 %q can take any character as delimeter. Syntatic sugar for #1
%Q/I'm a double quoted string/ #4 as can %Q. Syntatic sugar for #2
%!I am also a double quoted string! #5 if q/Q omitted, then Q is assumed.
my_string = <<END_OF_STRING
  Ruby also has heredoc syntax like perl
END_OF_STRING
# The delimiter must be the *only* thing on the line e.g.
  END_OF_STRING # A comment
# will not work

FIXME continue from here in the strings section

# think of %q as the "thin quote" and %Q as "thick quote"

# there are subtle differences between bracket chars and others e.g.
%q!the exact same char marks the start and end of this string literal!
%q{whereas here the string literal begins with an opening brace and ends with a closing brace}


%Q!The time is #{Time.now}! # variable interpolation works in these guys too

# Ranges
# ######

# Ranges are instances of the Range class

# There are 2 range operators
# .. creates a range that includes both the start and stop values
# ... creates a range that includes the start value but not the stop value
# Ruby implements 3 features with ranges: sequences, conditions, intervals

# Range methods
r = 3 .. 10 # initialize a new range
r.to_a # returns an array
r.to_enum # returns and Enumerator object
r.to_s # => "3..10" # returns a string represntation of the range - useful anytime?

('a'..'z').to_a # returns array of all lowercase alphabet chars
('A'..'Z').to_a # returns array of all uppercase alphabet chars

('a'..'Z').to_a # returns empty array (you can't mix case in range)
('a'..'9').to_a # returns empty array (you can't mix numbers and letters in a range)
('a'..'_').to_a # returns empty array (you can't do unusual stuff with a range!)

('aa'..'zz').to_a # returns array of aa, ab, ac --> zz


# Aside: converting a string to a range seems to involve extracting the first and last
# values as separate variables and building the range with the .. or ...
# operators as usual.

# a range can be strings, numbers, any object?

# Any ruby object can be used in a range provided it
# 1. implements a succ method that returns the next object in the range
# 2. implements <=> that compares two values and returns -1, 0, 1 depending on whether one is less than, equal, greather than the other

# Ranges as conditions (flip-flop)
# -------------------

# A range used in a boolean expression (an expression which returns true or false) has two states: set, unset. It is initially unset.
# .. and ... behave differently when used in boolean expressions!

# Used as a conditional, it starts evaluating true when the start point of the range is true and then evaluates false when the last part evaluates true

while line = gets
  puts line if (start-condition) .. (end-condition)
end

# Ranges as intervals
# -------------------

# the case equality operator (===) can be used to test whether a RHS value falls within the range on the LHS
1..10 === 5 # doesn't work as ruby thinks you are doing 1 .. (10 === 5)
(1..10) === 5 # => true
(1..10) === 3.14 # => true. If you mix Integers and floats, the result will be a float
('a'..'z') === 'y' # => true

# Variables
# #########

# naming conventions (not enforced by language) for variables

# Declare vs define vs initialize
# ###############################

# declare a variable - problably comes from strongly typed langs that need to
# know what type the variable is at compile time - they sort of say "reserve
# space for an Integer and call it i"

# in those strongly typed languages you also had to initialize ("give an initial
# value") to the variable or they would throw an error Define and initialize a
# local variable

# i think that the declare + initialzie steps are always combined in ruby
# ? is it possible to declare a variable in ruby without initializing it?

foo1 = 111

# a global varialbe
$foo2 = 222

# an instance var
@foo3 = 333

def afunc
  puts foo1 # NameError
  puts $foo2 # works
  puts @foo3 # works
end


# Class names, module names and constants *must* *start* with a capital letter. By
# convention contstants are all caps and modules, classes use studly caps e.g.
# 5 types of variables in ruby: constants, instance variables, local variables, class variables, global variables

# Hint: in ruby the 'end' keyword usually indicates scope change

# Constants
# #########

# Constants must begin with uppercase letter
# constants are available anywhere in the code no matter what scope they are defined in.
# constant defined outside a class or module is available anywhere unadorned
# constant defined within a class or module is available unadorned within the class/module. To access them from outside the class/module use the :: (scope operator) prefixed by any expression that returns the appropriate class/module object

#   constants in classes are state that is available to the whole program but namespaced to a particular class
#   they are a kind of "namespaced globally shared state "
#   i guess that even though they are changeable, they should be considered read only
#   ? should constants defined by a module/class be used outside that class? use cases?
#   they are probably fast as ruby can sort them out before run time

# constants may not be defined in methods
#   trying to do this throws a SyntaxError (dynamic constant assignment) this
#   seems to indicate that Ruby wants to know about all constants before it runs
#   the code (probably for speed reasons)

# constants can be added to existing classes from the outside
  class Foo
    CONST = 12

    def something
      CONST2 = 12 # throws a SyntaxError (dynamic constant assignment)
      puts "the const is #{CONST}"
    end

  end

  Foo::MY_CONST = 23 # works

  f = Foo.new
  f.class::CONST # => 12, works (any expresstion that returns the class will work)
  f::CONST # does not work (the scope resolution operator want a class/module object on LHS)


# constants in ruby can be changed (this will generate a warning)


# Global Variables
# ################

$global_var = 23 # Initializing a global variable

# global variables are available anywhere in the program

# referencing an uninitialized global variable returns nil (same as instance
# variables)-ruby seems very forgiving about globals and instance variables - it
# seems to say "meh i guess i just haven't seen the code that intializes this
# yet"

# Local Variables
# ###############

# A local variable is created dynamically when it is first assigned a value within a program
# a reference to an undefined local variable throws a NoMethodError
# ruby decides it's scope by limiting it ot the immediately enclosing
# 1. block or
# 2. method definition or
# 3. class definition or
# 4. module definition or
# 5. top level program

# So a local variable is created dynamically but statically scoped in ruby
# What are the implications of dynamic creation?
# * saves memory - only created when the code is actually run, not parsed
# * creates closures as it takes a reference to all the variables

# method parameters are considered to be local variables to that method definition
  def blah(alpha, beta, gaga)
    # alpha, beta, gaga are local to this method definition
  end

# block parameters are more complex. They are assigned values when the block is invoked
# if the local variable is first assigned in the block, it is local to the block
# if the local variable was previously defined in *the scope containing the blocks definition* then the block will share the variable with the scope except for
# 1. block parameters are *alwasy* local to the block
# 2. variables listed after a semicolon at the end of the block parameter list are always local to the block

  # Example:
  a = 1
  b = 2

  def foo(x,y)
    yield
  end

  foo("some", "thing") do
    # Because a and b were initialized in the same scope as this block, anything
    # we do to a and b in here will be permenant.

    # Notice that this block takeson the set of *local* variables in existance in the outside scope when it is created.
    # ? does the block get access to instance or class variables
    # these variables are forever bound to this block - it will have access to their current values when it executes
    # the binding preserves these variables even the original enclosign scope is destroyed
    # so a block is a closure???

    puts a
    puts b
    a = a + 1
    b += 1
    c = 12 # c is first initialized within the block so is scoped to this block
    puts a
    puts b
    puts c
  end

  # notice that a and b have been modified by the block
  puts a # => 2
  puts b # => 3
  puts c # error - c does not exist outside of the block

# Note that while, until, for loops do *not* create any change in scope in ruby

# Instance Variables
# ##################

# instance variables are available within instance methods throughout a class body

# referencing an uninitialized instance variable returns nil (not an error) - I
# think ruby just assumes it hasn't encountered the code that initializes the
# instance variable yet. This will produce a warning if you run ruby with -w

# each object has a unique set of instance variables

# instance variables hold the unique persistent state of each object (whereas
# class variables are a sort of "many objects global state") Example:

  class Foo
    @blah = 12
    def hello
      @blah
    end
  end
  f = Foo.new
  f.hello # => returns nil
# This doesn't throw any errors but the @blah = 12 assignment is never run so f.hello will alwasy return nil
# Notice that it wasn't illegal to refer to the variable even tho it had not been initialized

# Example:
  class Foo
    def someti
      @blah = 12
    end
    def hello
      @blah
    end
  end
  f = Foo.new
  f.hello # => returns nil, no error
  f.someti # => 12
  f.hello # => 12

class Foo
  def initialize
    @foo # declare an instance variable
  end
end
# Since initialize is the method run when we call Foo.new it makes sense to setup intial state in it

# Example:
  class Foo
    # how do we initialize an instance variable if it hasn't already been done?

    def thing
      unless defined? @myvar
        @myvar = "hello"
      end


# Class Variables
# ###############

@@foo4 # a class variable
# belongs to the innermost enclosing class *or* module
# shared between all instances of that class or module in the system
# class variables *must* be initialized before use

  # throws NameError exception because it is not initialized
  class Foo
    @@foo
  end

  # works
  class Foo
    @@foo = 1
    def goo
      @@foo = 1
    end
  end

  # works:
  class Foo
    @@foo # not initilazed here but is in the goo method FIXME why?
    def goo
      @@foo = 1
    end
  end

# class variables are only available within the class or module
# class variables used at the top level are defined in Object and function like global variables
# class variables are inherited by children but propagate upwards if defined in a child
# class variables do very non-obvious shit
  class Tesser
    @@eoin = 123

    def eoin
      @@eoin
    end

    def Tesser.eoin=(new_eoin)
      @@eoin = new_eoin
    end
  end

  # i guess this class variable is being added to Object which is an ancestor of my Tesser class
  @@eoin = "foo" # theoretically this shouldn't interact with @@eoin in Tesser

  ek = Tesser.new

  # it seems like the class variable in the g
  ek.eoin # => "foo"

  Tesser.eoin = 456
  ek.eoin # => 456

  # The @@eoin variable in Tesser is shared between all instances of Tesser and can be changed without an instance existing



# class variables are not recommended by pickaxe

Myconst # valid constant name but not usual.
CONST # valid and common constant naming

# Funny story, constants in ruby can be changed - they just generate a warning if you do
# There are no true constants in ruby

ClassName # valid class name
Class_name # valid but not usual

ModuleName # valid module name
Module__name # valid but not usual

# Ruby Pseudo-variables
# #####################

# self # the receiver object of the current method
  class Foo
    def me
      self.object_id # return the object id of this object
    end
  end

  f = Foo.new
  assert_equal f.object_id, f.me # true

# true # value representing true
# false # value representing false
# nil # ruby's "nothing" value
# __FILE__ # the name of the current source file
# __LINE__ # the current line number in the source file



# Math = ruby's built in math module. Math.sqrt(), Math.sin(), Math.cos()

# Symbols
# #######

# Creating a symbol
x = :hello # works
:"hello, this is actually a valid symbol name!!!" # also works (symbol names can be enclosed in quotes)

"astring".to_sym # returns :astring

:hello = x # does not work, you are not allowed to care about the value of a symbol i.e. you cannot assign values to it

# A symbol in ruby is an insance of the Symbol class
# A symbol variable is a reference to a Symbol object that contains the symbol name
# Symbol values cannot be changed
# You can find the symbol value by :thing.object_id

# i guess they are variables where we don't care what their value is
#
# symbols are words that begin with :
# they are cheaper than strings as ruby can store the symbol once and re-use it throughout your program
# useful for flags

# symbols are immutable objects - they are true constants in ruby
# symbols have a string representation and an integer representation - neither of which can be changed at run-time

# usage
# it is not recommended that you use symbols as "strings lite" as you have to call to_s on the symbol every time you want to use it
# symbol uses
# 1. naming keyword options in a method argument list
# 2. naming enumerated values
# 3. Naming options in an options tables hash
# ? where are symbols used in real code e.g. rails?



# a ruby symbol is a thing has both a number representation and a string representation - the string representation is used a lot more often.
# you can extract the string representaiton of a symbol from it at any time
# you can easily create a string with the same value as the symbols string representation - this is handy e.g.
attr_reader :foo # presumably ruby extracts the "foo" string to name the instance variable and method that attr_reader creates

# you can usually use a string anywhere in ruby you can use a symbol

# a symbol can be used to hold any immutable string you need in your code

# identifier = a variable, function or constant name
# in ruby there are methods that can look up identifiers at run time - these take symbols as arguments
# ruby symbols are handy if you only want to store one copy of a string in memory e.g. natural language processing
instance_variable_get
method
const_get
# useful for comparisions

:FOO == :FOO # ruby only had to test one thing
"FOO" == "FOO" # ruby had to test 3 things here (each char)

# symbols are good when you have a few unique string values and will use them as
# they are (no concatenation, uppercasing, lowercasing etc.) otherwise use strings

# does a symbol solve anything that a string literal couldn't do?

# there is a use-case for constants where we care that:
# * it's value doesn't clash with that of another constant (unique)
# * we don't care what that value actually is - it could be a number, string literal etc. in other languages we usually make it a number as these are the easiest for the interpreter/compiler to work with.
# * we care about whether the value will be evaluated true or false in conditionals
# * we care that the constant won't change it's value throughout the life of the program

# Example A

# Setup our constants so they are unique. We will never manipulate the values of these
  CELLO = 1
  CLARINET = 2
  DRUM = 3
  OBOE = 4

  players = {
    CELLO => "John",
    CLARINET => "Mary"
  }

# Example B (same thing with symbols)
  players = {
    cello: "John",
    clarinet: "Mary"
  }

# Example 2
NORTH = 1
SOUTH = 2
EAST = 3
WEST = 4

def move(direction)
  # here we get an integer that we use for comparison against itself but still
  # don't care about it's value.
  walk(90) if direction == NORTH
  walk(0) if direction == EAST
  walk(180) if direction == WEST
  walk(270) if direction == SOUTH
end
move(NORTH)

# or done with symbols
def move(direction)
  # here ruby creates each symbol when it first sees it - ruby takes care of making sure that the symbol will always have the same value
  walk(90) if direction == :north
  walk(0) if direction == :east
  walk(180) if direction == :west
  walk(270) if direction == :south
end
move(:north)

var $WP_DEBUG = "YES"

# symbols are stored internally as an integer

# symbols are simply constant names that you don't have to predeclare and are garuanteed to be unique

# you cannot apply a value to a symbol literal - ruby does it for you

# symbols are used instead of strings whenever we care about the identity and not the content

# symbols are good as keys in hashes especiall if the hashes are large or there
# are a lot of similar ones. In this case using a string lterial would use a lot
# more memory as each instance would have it's own string object for the key

# Example
# Consider class Foo
  class Foo
    def initialize
      @blah = { "name" => 'john', 'allocation' => '50 units'}
      # using string literals, the hash keys require 14 bytes of storage for each instance of Foo (assuming 8-bit strings) and each exta hash key requires as many bytes are there are characters in the key
      @blah = { name: 'john', allocation: '50 units'}
      # using symbols, the hash keys require 8 bytes of storage per instance of Foo (assuming 32 bit ints) and each extra hash key requires just 4 bytes
    end
  end
# If our system has 1 million Foo objects, we save 6 MiB of memory. This saving increases the more keys in the hash as each

# ? what char encoding are my ruby files, how does ruby represent strings internally - 8bit or 16bit?

# symbols make comparison faster as ruby only has to compare integers

# strings are garbage collected, symbols are not so any symbol I create will exists for the full lifetime of my program.

# symbols solve similar functions to constants

# ? What formula does ruby use for knowing what inteteger value a symbol should have?

# check out the ruby koans on symbols - i think that ruby creates a symbol with the same name as any constant you create in your program

stateflag = :ready
stateflag = :busy

# You can read the : in the symbol as "the thing named"

# you can think of symbols as integer constants that ruby makes easy for us
# can i think of symbols as "named integers"

# ? waht is the scope of symbols? global?
# So, strictly technically speaking, Symbols are pointers to memory objects containing the symbol name.









# Modules
# #######

#   serve 2 roles in ruby
#     1. group similar methods together under a familiar name to avoid name clashes
#     2. Share functionality between classes

# * You can't create objects out of modules (like classes can)
# * Classes can "mix in" a module which lets them use shared functionality without the formality of inheritance

# Methods (Functions)
# #################

def methodName
  puts "hello"
  # In general ruby methods return the value of their final statement
  # you can use return statement to be explicit
  return "bye"
end

# Implicit setter methods
def mysetter=(foo)
end

# anything listed after a method name is considered an *attachment*

# can bee called as
methodName
methodName() # the empty parenteses are not required but are good to control precedence if you are calling multiple functions
puts("hello".length)


# default parameters done like this:
def method2(name = "world")
  # can reference a variable in a string with #{varname}
  puts "hello #{name}"
  puts "hello #{name.capitalize}" # *everything* is an object in ruby

  # the value returned by a ruby method is the value of the last expression evaluated unless you explicitly use return
end

method2("this works")
method2 "so does this"

# anything listed after a method is considered an *attachment* e.g. print
# "hello" calls the print method and attaches the string object "hello".

# A ! at the end of a method name indicates that this method will modify the
# object it is called on. Ruby calls these "dangerous methods" as they change
# state that something else might have a reference to. Note that this is just a
# convention, not a rule! Some libraries/code do  not follow it well.
# It is a naming convention taken from scheme
"hello".downcase! # modifies the string itself rather than returning a copy of it

# Predicate naming convention (taken from scheme)
# Add ? to the name of any method that always returns a boolean value

def amIanInt?(thing)
  if thing.is_int
    return true
  else
    return false
  end
end

#ruby can chain methods

# Truth and False
# ###############

# true = ruby keyword for truth
# false = ruby keyword for false

# Only false and nil are false: 0, array() and "" are all true in conditionals.

# Classes
# #######

# A class is a combination of some state and some methods that use that state
# object == class instance

# In ruby methods are invoked by sending a message to an object
# <reciever>.method

# Creating a class
class Greeter
  def Initialize(name = "johnny")
    @name = name
  end
  def say_hi
    puts "hi #{@name}"
  end
  def say_bye
    puts "bye #{@name}"
  end
end


# The attr_accessor method does ???

# attr_accessor actually creates "get" and "set" methods - it does not create
# instance variables - these get created by the "setter" (var1=,var2= etc.)
# methods when they are called

class Foo
  # These are accessed on instances of this class as instance.var1 etc.
  attr_accessor :var1, :var2, :var3
end


# Type Conversion
# ###############

# ruby doesn't do automatic type conversion but provides chainable methods that do:
to_s() # convert to string
to_i() # convert to integer
to_a() # convert to array


# Arrays
# ######

# ruby arrays are ordered lists
# in an array, the key objects are always integer objects, in a hash, the key objects can be anything

# Hashes and arrays are not interchangeable

# Create a new array

myarr1 = Array.new # Invoke the Array constructor



myarr2 = [] # shortcut for Array.new

# arrays methods:
.max
.min

%w{ this is a weird thing } # creates an array with each word as an element

%w { this is a weird thing } # This doesn't work. why?

# Strings
# #######

# ruby objects like strings and numbers have the ability to find their own length and work on themselves etc.
# ruby treats single/double quotes simliar to how PHP does
# ruby only interprets backslashes e.g. \n, and does expression interpolation "hello #{varname}" in double quoted strings.

poem = 'a string'

# Strings are mutable
# ###################

# strings in ruby are mutable (unlike JS) but only some operators mutate them

str = "hello" # create a string object (String class instance)
str.object_id # => 20872440

str << " world" # mutates str - note that object_id stays the same
str.object_id # => 20872440

str = str + " world" # + returns a new object so str references a new object after this
str.object_id # => 19972380 (a new object id)


poem.lines.to_a.reverse # reverse the order of the lines in the string object 'poem'
# strings also have
# .length
# .lines
# .bytes
# .chars
# .downcase
# .delete

# Hashes
# ######

# a hash is an indexed collection
# any object can be the key!!!
# arrays and hashes grow as required  to hold new elements
# an array or hash can contian a mix of different types of ojects
# arrays are faster to access

# ruby has hashes (aka dictionaries)

myhash2 = Hash.new # invoke the Hash object constructor
myhash = {} # this is a shortcut for Hash.new

# Invoke the Array object constructor and tell it that the default value (what
# gets returned if I try to access an non-existing index) should be 0 not nil
myhash2 = Hash.new(0)


# hashes have functions
# .length # ruby often re-uses names (this is not the same method as array .lenght or string .length but does wha tyou expect)
# .keys
# .values

# Blocks
# ######

# you can make a block with {} or do, end keywords

{
  # i'm a block
}

do
  # so am I
end

# blocks are always attached to methods

# Filesystem Access
# ##############

Dir.entries "/path/to/dir" # returns an array of all the file names in /path/to/dir
Dir["/*.txt"] # return an array of all .txt files in / dir

# ? ruby does some sort of subsetting thing by calling an object with square brackets

stringy = "this is a blow out"
stringy["blow"] = "cop" # replaces ?first/all instances of "blow" with "cop"

Dir
File
FileUtils.cp # unix copy thingy

# can load another file with code in it using require

require 'mylib'

# Date & Time
Time.now # current time & date

# Code blocks
# ###########

def oi_func(p1, p2)
  puts "doing stuff"
  yield(p1,p2)
  puts "doing more stuff"
end

oi_func('hello','there') { |first, second| puts "this is the #{{first} and this is the #{second}"}


# rdoc tries to leave the ruby code looking as natural as possible

# you can use

=begin rdoc
This rdoc comment is delimted by the =begin and =end but this style is not commonly used.
=end
def myfunc
  # do blah
end

# rdoc will pull this comment out and use it as the description of the  funky2
# method. In rdoc paragraphs are lines that share a left margin. Anything
# indented will be displayed as is. Words can be displayed in _italic_ or *bold* or +monospaced+
#--
# rdoc will ignore any comment between #-- and #++ (allows internal comments)
#++

def funky2
  # do stuff
end


while condition

end
# Lesson: Methods introduce a new variable scope.

# Unlike in JS, variables declared in the same static scope as a method are
# *not* available inside the method.

  x = 3 # returns 3
  def foo
    # This will fail as outside x not available within this function
    y = x + 3 # returns NoMethodError
    # In here x is nil
  end
  puts x # outputs 3, returns nil


# while, until, for loops do *not* introduce a new varialbe scope.

  x = 10
  while foo
    # This works as the while loop does not introduce new scope
    puts x # outputs x, returns nil
    y = x + 1
  end

  # any variable first seen within the while is available after it as the while
  # loop does not introduce new scope.
  puts y # outputs 11, returns nil

# can use statement modifiers to make a do while


begin
  puts "doing stuff"
end while condition # the block always gets executed at least once

#contrast this with

puts "doing stuff" while condition # this is just a re-write of a traditional while-loop, not a do-while

# blocks and methods have different scoping rules

# Create a variable
# #################

# creates local variable x
# by default this is only available within the current method

# ? does ruby have  an undefined
# a variable is undefined if it doesn't exist?

# a variable which is set to nil *is* initialized!
# is there a way in ruby to create a variable but not initialize it?

# set x to 33 if x evaluates to false or nil
# Do not use this form to initialize boolean vars (obviously false is a legitimate value for these)
x  = x || 33
x ||= 33 # short-cut version of sam

# set z t0 44 if z evaluates to nil (does not check that z is false) - this makes this suitable for initializing booleans
z = 44 if z.nil?


# at parse time, ruby creates *local* variables whether the code is executed or not. So x is created by ruby and set to nil at parse time.
# now when ruby comes to evaluate this line, it has x = nil so defined?(x) returns "nil" which means the assignment expression is never executed.
x =  44 unless defined?(x) # does not work

# apparently this behaviour is not the same for class variables
@x = 44 unless defined?(@x) # works

# Validating Data

# validate data when it enters the system but not after as you will have to test  it in every method etc. and all that extra code is a potential source of errors.
# have a "guardian" that validates input - otherwise is not DRY
# if you guard everwhere you are making all your methods bigger
# cell analogy - cells spend a large % of their energy in keeping the outside, outside

# = Chapter 8: Methods
# ====================

# Methods in ruby should be short

# Is ruby pass by value or pass by reference?
# ###########################################

# Everything in ruby is an object so all variables are actually references to objects
# These references are passed by value to functions
# Languages that pass by reference are still passing the value of the reference by value

# Ruby used copy-on-write

# Copy on Write
# #############

  # If there are multiple variables referencing the same object, they all
  # reference the exact same object until they try to modify it, then ruby
  # sneakily creates a new copy for the caller. The win here is that if none of
  # the objects want to write to it, then you can share the single instance
  # between all of them - this saves memory

# variables in ruby are essentially pointers that contain the address of their
# object.

# Integers are a special case in ruby - for speed they *are* the whole object (fits in 32 bits)


a = []
b = a
a.object_id # => 21858108
b.object_id # => 21858108

# so a and b are both references to the same array object

def swap(a, b)
  # in here a and b are the parameters passed to the function. These are different to the outside a, b bu they do reference the same array

  # Look how the a and b in here reference the same objects as outside:
  a.object_id
  b.object_id

  # but if we alter the a and b in here, the outside a, b are not changed
  a = "hello"
  b = "foo"

  # see how a and b in here now reference different objects
  a.object_id
  b.object_id
end

# but the a and b at this level are not changed
a.object_id
b.object_id

# so ruby passes variables by value but all values in ruby are themselves references to objects

# the implication of this pass by reference stuff is that you can't have  a method alter a variable outside it's scope

def foo(x)
  x += 1
end

m = 3

# This method will neer work in ruby - even though other languages that do "pass by reference" would change this
foo(m) # returns 4, does not alter m

m = foo(m) # this returns 4 and gets assigned to m

# for example in PHP
<?php
# this function signature indicates that it accpets a reference to a variable
function foo(&$x) {
  $x += 1
}
$m = 3
foo($m) # this will alter m
echo $m # prints 4
?>

# so you can think of ruby as being *only* "pass by value" if you use "pass by reference" in the PHP sense

# Consider this code:
  a  = "foo"
  b = "bar"

  puts "a = " + a.object_id.to_s
  puts "b = " + b.object_id.to_s

  def ghi(a,b)
    puts "a = " + a.object_id.to_s
    puts "b = " + b.object_id.to_s
  end

  ghi(a,b)

  puts "a = " + a.object_id.to_s
  puts "b = " + b.object_id.to_s

# Here we see that the a and b within the ghi() method actually refer to the same
# objects as the a and b outside. This is ruby shadowning those variables in the
# method - if they are writtent to within the method they get new values but if
# they are just read the the outside values are used


# Naming Methods
# ==============

# Method names must start with a lowercase letter or underscore - method names cannot start with numbers!
foofoo # correct
Foofoo # will be defined ok, will throw NameError when you call it
32foofoo # wrong. Throws SyntaxError exception when you try to define it
_foofoo # correct

# Method names can include !,=, ?. There are conventions surrounding their use

# == Predicate Methods (names ending in ?)
# ? appended to a method name implies that the method returns boolean
# (ruby does not enforce this - this is just a human convention)

# == Bang Methods
# ! appended to a method name implies that this method is "dangerous" in some
# way, usually because it modifies it's receiver. Again the ruby interpreter
# does not treat these methods any differently - it is just a human convention

# bang method name implies that this method does something descrutive to the receiver

# In ruby core & stdlib a bang method is always be paired with a "non dangerous"
# version e.g. foo() and foo!()

# See: http://dablog.rubypal.com/2007/8/15/bang-methods-or-danger-will-rubyist

# *many* but not *all* methods in ruby core and stdlibs marked with ! change
# *their recievers so don't read it that way.

# * Not every receiver changing method ends in ! e.g. Array#pop/push/shift/unshift/concat/clear
# * Not every ! method changes it's receiver e.g. FIXME?

# Bang and non-bang methods should be defined in pairs & to define the non-bang
# one in terms of the bang (it can be done other way round but this is the
# convention in the ruby source)

class Thing
  def initialize
    @yoke = ['a','b','c']
  end

  # modifies the instance variable forever
  def flatten!
    @yoke = @yoke.flatten
  end

  # returns the modified version of the instance variable but leaves the original alone
  # defined in terms of the bang version
  def flatten
    dup = @yoke.clone
    dup.flatten!
  end
end


# he believes you should read ! as "the dangerous *version* of this method" not "this method is dangerous"

# == Setter Methods FIXME wrong name?
# = appended to method name is used as a setter (ruby interpreter uses this)

class Car
  def initialize(num_doors, num_wheels)
    @doors = num_doors
    @wheels = num_wheels
  end

  def haswheels?
    if @wheels > 0
      return true
    else
      return false
    end
  end

  def spinnythings=(num_spinnythings)
    @wheels = num_spinnythings
  end

  def roundthings(num_roundthings)
    @wheels = num_roundthings
  end

  def to_s
    puts "This car has #{@wheels} wheels and #{@doors} doors"
  end
end

subaru = Car.new(4,4)

# When we put a method call on the LHS of an assigment, it seems that ruby
# automatically appends = to the name when it looks for it in the receiver
# object.

subaru.spinnythings=(7) # Works, returns 7. This is just us calling the method in the usual way
subaru.spinnythings = 6 # Works, returns 6. here ruby sees the method on the LHS so searches for spinnythings= in the class
subaru.roundthings = 5 # Fails. Returns NoMethodError as ruby looks for and fails to find subaru.roundthings= method

# So this really is just a "convention" built into the language
# If ruby sees a method call on the LHS of an assignment it will silently append '=' to the name and call that in the receiver with the RHS as parameter.

subaru.spinnythings = 10 # becomes subaru.spinnythings=(10)


# Starting a method with an uppercase

def FooFoo
  puts "hello"
end

FooFoo # throws NameError exception as it can't find the constant FooFoo
FooFoo() # works

def booBoo
  puts "hello"
end

booBoo # works
booBoo() #  works

# The ruby interpreter has a notion that things that begin with a uppercase letter are a constant unless you explicitly tell it otherwise ?

# By convension method names starting with uppercase letter are used for type conversion e.g.
Integer("33") # converts "33" to 33

# Arguments vs parameters
# #######################

  def my_func(i_am_a_parameter) # a parameter is any declaration within the parentheses in a function definition
    # do stuff
    puts "Parameter is a variable in the *definition* of the function"
    puts "Argument is the actual value of the variable that gets passed to the function"
    puts "arguments are passed as parameters"
    puts "#{i_am_a_parameter}"
  end

  i_am_an_argument = 10
  my_func(i_am_an_argument) # an argument is any expression within the parentheses of a function call

# Method arguments

func1  # this is the prefered way to call a method with no arguments
func1() # this is legal but not usually done in ruby

# ruby does default parameters like this:
myfunc(param1 = "foo", param2 = "blah", param3 = "goo")
# The parameters can be any ruby expression and can include references to previous parameters in the list
myfunc2(p1 = 33, p2 = p1 * 2, p3 = p2 * 2) # legal

# You can add defaults to any parameter in almost any order
metty(p1 = 2, p2, p3) # 1st arg has default value. works
metty(p1, p2 = 22, p3) # 2nd arg has default value. works
metty(p1, p2, p3 = 33) # 3rd arg has default value. works
metty(p1 = 2, p2 = 4, p3) # first 2 args with default values. works
metty(p1 = 2, p2, p3 = 4) # does not work

# default parameters let you do variable length argument lists

# Splat Parameters
# ################

# prefix a parameter name with * indicates "hey ruby i would like you to capture
# multiple arguments, bundle them into an array and assign them to this
# paramter.
def metty(p1, *the_rest)
  p p1
  p the_rest
end

# you can only have one spalt argument in the parameter list (would be ambigious otherwise)
# the splat argument can be anywhere in the list. ruby will make sure the other args get filled first and the splat gets the leftovers

  def metty(p1, *leftovers, p2)
    # do stuff
  end
  metty(1,2) # p1 = 1, p2 = 2, leftovers = []
  metty(1,2,3,4) # p1 = 1, p2 = 4, leftovers = [3,4]

  # if you mix default arguments and splat arguments, the default args will get filled first


  # you  must put all parameters with defaults before the splat argument
  def metty(p1 p2 = 3, *others)
  end

  def metty(p1, *others, p3 = 4) # does not work
  end

# in the real world it's probably a bad idea to mix splat args and default values

# types of parameters that a method can have
# 1. must be there/required. the variable must exist and value must be supplied by calling code at run-time (required values)
# 2. variable must exist but value can be supplied at run-time or hard-coded default value (default values)
# 3. variable does not even have to exist and none of the method code depends on it's value or existance  (splat args)


# Passing a variable number of parameteers to a method pattern

# option 1
def metty(params = {})
  # params is a named collection of parameters
  # - lookups are a bit more verbose e.g. params[:name]
  # - it's not obvious how many parameters this function gets/requires
  # - adding a default value to parameters requires a bit more code (see below). I get this for free if I use ruby's default values.
  # - it is a lot more verbose to say that the method *requires* a certain parameter - I have to check for it manually at run-time - the interpreter can't help me
  # + each parameter is accessed by it's name
  # + parameters are not dependant on order passed to function
  # + you can just supply the parameters you want, since order doesn't matter you don't have to restate the first one to get the second
  # ? how does this work out with block parameters?
  params[:name] = 12

  # this emulates the default values functionality of ruby
  params = { name: "foo", skill: "blah" }.merge(params)

  # do stuff
end

# option 2
def metty(*params)
  # params is an array of parameters
  # - params are dependant on order passed
  # - params are accessed by position in the array, not by name (less obvious what each parameter is)
  # ? are arrays faster than hashes in ruby? if so, would it matter in this case?
  # ? how does this work out with block parameters?

  # do stuff
end

# We define this metty with 2 required parameters
def metty(reqd, params)
end

# We pass metty a string and a hash as it's required parameters
metty('song', { :genre => 'jazz', :length => '180'})

# this is a common pattern so ruby helps by allowing us to remoce the {}
metty('song', :genre => 'jazz', :length => '180') # ruby will collect all the key => value arguments into a single hash
# the rules for this is that the key => value pairs must
# 1. follow any normal arguments
# 2. be before any splat or default arguments

# In ruby 1.9 the new hash literal syntax allows us to write (this is one of the reasons for the new syntax)
metty('song', genre: 'jazz', length: '180')


# block parameters
# ################

# in ruby you can associate a block with a method - the method can yield control to that block using the yield statement
# however you *can* "name" the block and get access to it as a Proc object

# the block parameter must begin with & and must be the last parameter in the list
def foo(p1, p2, foo, &meinbloc)

  # now meinbloc is a Proc object that I can pass around to other methods etc.
  meinbloc.call # executes the meinbloc code
end

foo(1,2,3) { puts "this block gets put in the meinbloc parameter" }

# ruby lets you convert block to Proc if that is more useful (see above) and
# also lets you pass a Proc and convert it to a block if that is waht the
# function expects

# General format of a ruby method call
# <optional receiver>.method_name <optional parameters> <optional block>

# If you omit the reciever it defaults to 'self', the current object. This allows methods within a class to call each other by name
# If you invoke a method that isn't part of a class, it appears that self = main within the method
# it seems that 'self' in ruby is has a value depending on the current scope


class Child < Parent
  def initialize(*stuff)
    # do stuff here that does not require the stuff array
    # super keyword invokes a method in the parent class (super class) with the same name as the method it is invoked from.
    # if it doesn't find the method in the immediate parent, it will search up the inheritance chain
    super # calling super like this will pass it all the args we received
  end
end

# ruby allows us to make a special case here (we don't use the args in
# initialize anyway so there is no particular advantage in being able to refer
# to them from it)
class Child < Parent
  def initialize(*)
    # do stuff here that does not require the stuff array
    super # calling super like this will pass it all the args we received
  end
end

# Since everything in ruby is an object, there is no "pass by value" -
# everything is a reference so everything is "pass by reference". However many
# methods e.g. + will return a new object which can make it seem like they
# modified the old one.

# Splat Arguments
# ###############

# You can use splats in arguments (when calling a function)
foo(32, "foo", *[3, 44, 54])
# It will convert any collection or enumerable object into its individual
# elements and pass each of those elemetns as a separate parameter to the
# function.

# splat arguments can appear anywhere in the parameter list
foo(*(10 .. 20)) # passes 10, 11, 12 -> 19, 20

# Proc Arguments
# ##############

# If the last argument in a method call is preceeded by & ruby assumes it is a
# Proc object, removes it from the parameter list, converts it to a block and
# associates it with the function

  l = lambda { puts "hello" }
  foo(23, 45, &l)

  # is exactly the same as
  foo(23, 45) { puts "hello" }

# Faking Named Parameters
# #######################

# In ruby you can omit the braces on a hash passed as parameter to a method but
# only if the hash is the last parameter. Rails uses this a lot.

do_foo_thing name: 'Eoin', age: 33
do_foo_thing(name: 'Eoin', age: 33) # same thing
do_foo_thing({ name: 'Eoin', age: 33 }) # same thing

# Returning stuff from ruby methods
# #################################

# Every method in ruby returns a value.

# Unless you have an explicit return statement, ruby returns the result of the
# last expression evaluated in a method.

# It is idiomatic in ruby to omit the return statment unless one is required.

# Some folks suggest leaving a empty line before an explicit return statment to
# indicate it's "specialness"

# PROS of explicit returns
# An explicit return statement is good to indicate to other programmers "I am deliberately returning this value"

# CONS of explicit returns
# like gotos, make it harder to follow flow control

# splats in return statements

# Tasks you might want to do with returning a value
# * return nothing (impossible in ruby - however you can ignore the return value)
# * return a single value
# * return multiple values (can return an array - ruby has shortcut syntax for this)
# * return named values (can return a single hash)

def return0
  # A string literal in double quotes returns the string object
  "I get returned from this function as i am what returns from the last statement in the func"
end

def return1
  # puts will return nil so this function also returns nil
  # contrast this with just evaluating the string in return0 above
  puts "foo"
end

def return2
  puts "foo"

  return "blah" # notice the empty line before the return statement
  # this function returns the string object "blah"
end

def return3
  x = "foo" # assignment in ruby returns the value assigned
  # this method returns  the string object "foo" i.e. the result of evaluating the RHS of the assignment
  # because assignments return their RHS value you can chain them and use them in some unexpected places FIXME elaborate
end

# Incidentally, the def -> end block is an expression that returns nil


def return4

  # We create and return an array
  arry = [33, "foo", "bar"]
  return arry


  # doing the exact same thing using ruby shortcut syntax
  return 33, "foo", "bar"

  # this will also return the array (provided it is the last expression in the function)
  [33, "foo", "bar"]

end

r1, r2, r3 = return4 # multiple assignment will collect the array of 3 values from return4 and assign them to the rX variables

# Inspecting a ruby object
thing.class # returns the class of it's receiver. must always be called with an explicit receiver
thing.inspect # returns a string containing a human readable version of thing (calls thing's to_s if )
thing.methods # lists all it's methods as an arry of symbols - includes all symbols it inherits
things.methods - Object.methods # don't show the methods that thing gets from Object


# Code Style

# Josh Susser has some good thoughts on when to use parens in ruby method defnitions and invocations.

# Definitions
# * Never use parens when defning methods with no arguments
# * Always use parens when defining methods with arguments

# Invocations
# * Never use parens when sending a message with no arguments
# * In general, use parens when sending a message with arguments
# * Always use parens when sending a message if using it's resulting value (assigning to a variable or passing as an argument)
# * Optionally omit parens when sending a message with arguments if it's in a DSL or the resulting value is ignored

# He does this so that the method invocations can be more easily chained when refactoring later

# Exceptions
# ##########

begin
  # do stuff
rescue Exception => ex
  # do stuff with ex
end

# You can omit the begin and end statements if this block is the top-level block within a function e.g.

def doStuff
    #do stuff
  rescue Exception => ex
    # do stuff with ex
end
