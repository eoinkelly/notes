---
layout: post
title: 'Feeding a Ruby Function'
date: 2012-08-10 19:35
comments: true
categories:
published: false
---

# = Chapter 8: Methods

# ====================

# Methods in ruby should be short

# Naming Methods

# ==============

# Method names must start with alpha-numeric or underscore

# Method names can include !,=, ?. There are conventions surrounding their use

# == Predicate Methods (names ending in ?)

# ? appended to a method name implies that the method returns boolean

# (ruby does not enforce this - this is just a human convention)

# == Bang Methods

# ! appended to a method name implies that this method is "dangerous" in some

# way, usually because it modifies it's receiver. Again the ruby interpreter

# does not treat these methods any differently - it is just a human convention

# In ruby core & stdlib a bang method is always be paired with a "non dangerous"

# version e.g. foo() and foo!()

# See: http://dablog.rubypal.com/2007/8/15/bang-methods-or-danger-will-rubyist

# _many_ but not _all_ methods in ruby core and stdlibs marked with ! change

# \*their recievers so don't read it that way.

# \* Not every receiver changing method ends in ! e.g. Array#pop/push/shift/unshift/concat/clear

# \* Not every ! method changes it's receiver e.g. FIXME?

# Bang and non-bang methods should be defined in pairs & to define the non-bang

# one in terms of the bang (it can be done other way round but this is the

# convention in the ruby source)

class Thing def initialize @yoke = ['a','b','c'] end

# modifies the instance variable forever

def flatten! @yoke = @yoke.flatten end

# returns the modified version of the instance variable but leaves the original alone

# defined in terms of the bang version

def flatten dup = @yoke.clone dup.flatten! end end

# he believes you should read ! as "the dangerous _version_ of this method" not "this method is dangerous"

# == Setter Methods FIXME wrong name?

# = appended to method name is used as a setter (ruby interpreter uses this)

class Car def initialize(num_doors, num_wheels) @doors = num_doors @wheels =
num_wheels end

def haswheels? if @wheels > 0 return true else return false end end

def spinnythings=(num_spinnythings) @wheels = num_spinnythings end

def roundthings(num_roundthings) @wheels = num_roundthings end

def to_s puts "This car has #{@wheels} wheels and #{@doors} doors" end end

subaru = Car.new(4,4)

# When we put a method call on the LHS of an assigment, it seems that ruby

# automatically appends = to the name when it looks for it in the receiver

# object.

subaru.spinnythings=(7) # Works, returns 7. This is just us calling the method
in the usual way subaru.spinnythings = 6 # Works, returns 6. here ruby sees the
method on the LHS so searches for spinnythings= in the class subaru.roundthings
= 5 # Fails. Returns NoMethodError as ruby looks for and fails to find
subaru.roundthings= method

# So this really is just a "convention" built into the language

# If ruby sees a method call on the LHS of an assignment it will silently append '=' to the name and call that in the receiver with the RHS as parameter.

subaru.spinnythings = 10 # becomes subaru.spinnythings=(10)

# Starting a method with an uppercase

def FooFoo puts "hello" end

FooFoo # throws NameError exception as it can't find the constant FooFoo
FooFoo() # works

def booBoo puts "hello" end

booBoo # works booBoo() # works

# The ruby interpreter has a notion that things that begin with a uppercase letter are a constant unless you explicitly tell it otherwise ?

# By convension method names starting with uppercase letter are used for type conversion e.g.

Integer("33") # converts "33" to 33

# Arguments vs parameters

#

def my_func(i_am_a_parameter) # a parameter is any declaration within the
parentheses in a function definition # do stuff puts "Parameter is a variable in
the _definition_ of the function" puts "Argument is the actual value of the
variable that gets passed to the function" puts "arguments are passed as
parameters" puts "#{i_am_a_parameter}" end

i_am_an_argument = 10 my_func(i_am_an_argument) # an argument is any expression
within the parentheses of a function call

# Method arguments

func1 # this is the prefered way to call a method with no arguments func1() #
this is legal but not usually done in ruby

# ruby does default parameters like this:

myfunc(param1 = "foo", param2 = "blah", param3 = "goo")

# The parameters can be any ruby expression and can include references to previous parameters in the list

myfunc2(p1 = 33, p2 = p1 _ 2, p3 = p2 _ 2) # legal

# You can add defaults to any parameter in almost any order

metty(p1 = 2, p2, p3) # 1st arg has default value. works metty(p1, p2 = 22,
p3) # 2nd arg has default value. works metty(p1, p2, p3 = 33) # 3rd arg has
default value. works metty(p1 = 2, p2 = 4, p3) # first 2 args with default
values. works metty(p1 = 2, p2, p3 = 4) # does not work

# default parameters let you do variable length argument lists

# Splat Parameters

#

# prefix a parameter name with \* indicates "hey ruby i would like you to capture

# multiple arguments, bundle them into an array and assign them to this

# paramter.

def metty(p1, \*the_rest) p p1 p the_rest end

# you can only have one spalt argument in the parameter list (would be ambigious otherwise)

# the splat argument can be anywhere in the list. ruby will make sure the other args get filled first and the splat gets the leftovers

def metty(p1, \*leftovers, p2) # do stuff end metty(1,2) # p1 = 1, p2 = 2,
leftovers = [] metty(1,2,3,4) # p1 = 1, p2 = 4, leftovers = [3,4]

# if you mix default arguments and splat arguments, the default args will get filled first

# you must put all parameters with defaults before the splat argument

def metty(p1 p2 = 3, \*others) end

def metty(p1, \*others, p3 = 4) # does not work end

# in the real world it's probably a bad idea to mix splat args and default values

# types of parameters that a method can have

# 1. must be there/required. the variable must exist and value must be supplied by calling code at run-time (required values)

# 2. variable must exist but value can be supplied at run-time or hard-coded default value (default values)

# 3. variable does not even have to exist and none of the method code depends on it's value or existance (splat args)

# Passing a variable number of parameteers to a method pattern

# option 1

def metty(params = {})

# params is a named collection of parameters

# - lookups are a bit more verbose e.g. params[:name]

# - it's not obvious how many parameters this function gets/requires

# - adding a default value to parameters requires a bit more code (see below). I get this for free if I use ruby's default values.

# - it is a lot more verbose to say that the method _requires_ a certain parameter - I have to check for it manually at run-time - the interpreter can't help me

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

def metty(\*params)

# params is an array of parameters

# - params are dependant on order passed

# - params are accessed by position in the array, not by name (less obvious what each parameter is)

# ? are arrays faster than hashes in ruby? if so, would it matter in this case?

# ? how does this work out with block parameters?

# do stuff

end

# We define this metty with 2 required parameters

def metty(reqd, params) end

# We pass metty a string and a hash as it's required parameters

metty('song', { :genre => 'jazz', :length => '180'})

# this is a common pattern so ruby helps by allowing us to remoce the {}

metty('song', :genre => 'jazz', :length => '180') # ruby will collect all the
key => value arguments into a single hash

# the rules for this is that the key => value pairs must

# 1. follow any normal arguments

# 2. be before any splat or default arguments

# In ruby 1.9 the new hash literal syntax allows us to write (this is one of the reasons for the new syntax)

metty('song', genre: 'jazz', length: '180')

# block parameters

#

# in ruby you can associate a block with a method - the method can yield control to that block using the yield statement

# however you _can_ "name" the block and get access to it as a Proc object

# the block parameter must begin with & and must be the last parameter in the list

def foo(p1, p2, foo, &meinbloc)

# now meinbloc is a Proc object that I can pass around to other methods etc.

meinbloc.call # executes the meinbloc code end

foo(1,2,3) { puts "this block gets put in the meinbloc parameter" }

# ruby lets you convert block to Proc if that is more useful (see above) and

# also lets you pass a Proc and convert it to a block if that is waht the

# function expects

# General format of a ruby method call

# <optional receiver>.method_name <optional parameters> <optional block>

# If you omit the reciever it defaults to 'self', the current object. This allows methods within a class to call each other by name

# If you invoke a method that isn't part of a class, it appears that self = main within the method

# it seems that 'self' in ruby is has a value depending on the current scope

class Child < Parent def initialize(\*stuff) # do stuff here that does not
require the stuff array # super keyword invokes a method in the parent class
(super class) with the same name as the method it is invoked from. # if it
doesn't find the method in the immediate parent, it will search up the
inheritance chain super # calling super like this will pass it all the args we
received end end

# ruby allows us to make a special case here (we don't use the args in

# initialize anyway so there is no particular advantage in being able to refer

# to them from it)

class Child < Parent def initialize(\*) # do stuff here that does not require
the stuff array super # calling super like this will pass it all the args we
received end end

# Splat Arguments

#

# You can use splats in arguments (when calling a function)

foo(32, "foo", \*[3, 44, 54])

# It will convert any collection or enumerable object into its individual

# elements and pass each of those elemetns as a separate parameter to the

# function.

# splat arguments can appear anywhere in the parameter list

foo(\*(10 .. 20)) # passes 10, 11, 12 -> 19, 20

# Proc Arguments

#

# If the last argument in a method call is preceeded by & ruby assumes it is a

# Proc object, removes it from the parameter list, converts it to a block and

# associates it with the function

l = lambda { puts "hello" } foo(23, 45, &l)

# is exactly the same as

foo(23, 45) { puts "hello" }
