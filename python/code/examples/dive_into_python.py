"""
If the first thing in a module is a multi-line string it is treated as a doc
string.
"""

# print out our own doc string
# print(self.__doc__) # not working
# TODO: how to get a reference to the current module

# pylint considers any variable declared outside a function should be either a
# class name or a constant so will complain about this:
blah = "hello"

if blah == "hello":
    print("do stuff")

if blah != "hello":
    print("other stuff")

# None is the python null value
# functions return None if you don't explicitly return something


# default args look like ruby
def do_things(a_thing, other="hello"):
    """I am the do_things function
    hear me roar!
    """
    if other == "hello":
        print("we used default b")
    else:
        print("we passed a b")

    print(a_thing)

    if a_thing == "die":
        # exceptions look like ruby
        # what is the exception heirarchy?
        raise ValueError("bad thing")


# docstring is available as the __doc__ attribute of a function object
print(do_things.__doc__)

# invoking function that has default args
do_things("i am a")
do_things("i am a", "i am b")
# do_things('die') # will raise exceptoin

# when you import a module you get its *public*
# 1. classes
# 2. attributes
# 3. functions
#
# importing the module does not bring those things into your namespace - you
# have to refer to them via sys.foo sys.bar etc.
import sys

# sys.path is an "attribute" of the sys module
# sys.path represents the search path python will use to find modules
# many modules are built-in to python interperter (batteries included approach)
# so not everything comes from a .py file

print(sys.path)

# everything in python is an object - including functions


# "code blocks" in python are defined by their indentation
# Notice that
# * if
# * def
# * for
# * else
# all start code blocks and all end in `:`

# python arrays look like ruby
things = ["a", "b", "d", "e", "g"]

for x in things:
    print(x)

# python loves exceptions

try:
    raise ValueError("bad")
except ValueError:
    print("got bad value")

# python will not let you reference a variable that has not been assigned a
# value! Nice!
# print(x)
# x = 12

# QUESTION: why can't I rescue a nameerror?
# try:
#     print('trying...')
#     print(x)
#     # x = 12
# except NameError:
#     print('naughty')


# all modules have a __name__ attribute
#
# * name will be __main__ if you run the module directly
# * name will be the module's filename (without path or extension) if you
# import it
#
# so you can use __name__ to decide whether we are being imported or run
# directly
# This can be handy for running unit tests on the module (used in practice???)
if __name__ == "__main__":
    print("we are main")

# Python datatypes
#
# * Boolean
#     * it has the notion of truthy and falsy in if expressions i.e. other data
#       values will evaluage to either True or False in conditional expressions
#     * Booleans can be treated as numbers: True is 1, False is 0 (legacy py2
#       stuff)
# * Number
#     * same type for ints and floats (uses decimal point to distinguish)
#     * examples (notice types are written in lowercase)
#         * int
#         * float
# * String
# * Byte ??
# * Byte array ???
# * Lists
#     * ordered
#     * work pretty similar to ruby lists (e.g. + concatenates them)
#     * hetreogenous
# * Tuples
#    * ordered, immutable, bit faster than regular lists
#    * can be used as dictionary keys (unlike regular mutable lists)
#    * immuatable lists (defined with () not [])
#    * hetreogenous
#    * can be used for multiple assignement e.g. `(x, y, z) = v`
# * Sets
#     * unordered
#     * hetreogenous
#     * defined with {} e.g. {1, 4, 45, True, 'hi'}
my_set = {1, 23, "hi", True, False}

# * Dictionaries
#     * unordered, key value pairs
#     * dictionary values can be any type
#     * keys can be strings, numbers, tuples, others???
#     * keys can have different types

my_dict = {"name": "dictionary value Eoin", "age": 33}
print(my_dict["name"])

# * None
#     * the only instance of NoneType
#     * the null value

# Other types
#
# * Function
# * Class
# * Method
# * Compiled code (interesting!)
# * File

nummy = 12.3

# type() gets type of any value
print(type(nummy))

if isinstance(nummy, float):
    print("we got a float")
else:
    print("not a float")

# up to http://www.diveintopython3.net/comprehensions.html


# classes


class FooBar:
    pass
    # pass is a noop statement handy when stubbing out stuff, probably required
    # so that you can have empty code block


class Car:
    """I am a car factory class"""

    # init works same as ruby, is called after memory allocation
    def __init__(self, make, model):
        """ """
        self.make = make
        self.model = model

    def drive(self):
        """I drive"""
        print("bee beep from {make} {model}".format(make=self.make, model=self.model))


# instantiate by calling the class name as a constructor passing whatever
# __init__ requires
my_car = Car("toyota", "yaris")
my_car.drive()
print(my_car.__class__)
print(my_car.__doc__)


# instance variables are public in python
print(my_car.make)
print(my_car.model)


# they do seem to have a way to have my_car.foo actually run a getter transparently (and similarly for my_car.foo = 'blah' to run a setter)

# TODO: lookup @property and @attr annotations

# there is an _prefix convention indicating privacy but it is not enforced

# python and the filesystem
# ##########################

# os contains all the "operating stuff"
import os

print(os.getcwd())  # cwd of python process

os.path
# a bunch of functions for manipulating paths and dirs
new_path = os.path.join("/some/bar", "foo.py")
print(new_path)

# glob does shell glob expansion
import glob

# comprehensions
# ##############

# a really neat way to
# * turn one array/dictionary/set into another
# * filter an array/dictionary/set

a_list = [1, 9, 8, 4]
new_list = [el * 3 for el in a_list]
print(new_list)
# => [3, 27, 24, 12]

print([el * 3 for el in a_list if el > 2])
# [27, 24, 12]

# A dictionary comprehension is like a list comprehension, but it constructs a dictionary instead of a list.

maybe_dict = {el: el * 3 for el in a_list if el > 2}
# {8: 24, 9: 27, 4: 12}

print(type(maybe_dict))
# <class 'dict'>

print(maybe_dict[9])
# 27

# python also has set comprehensions

# watch out - the syntax is similar to dictionary comprehensions

a_set = set(range(10))
# {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}

print({x * 3 for x in a_set})
# {0, 3, 6, 9, 12, 15, 18, 21, 24, 27}


# You can turn any class into a "callable" (a lambda) by implementing __call__()
class Funky:
    """my instances can be used just like a function"""

    def __call__(self):
        print("hi there from funky the callable")


fun = Funky()
fun()


# with

# * takes a code block
# * creates a "runtime context"
# * allows python to implement before and after setup and cleanup
# * similar to blocks in ruby
with open("/path/some/file.txt") as some_file:
    some_file.seek(4)
    char = some_file.read(1)

# python objects have destructors (implement __del__()) but it is not straightforward to understand when they are called (due to GC)


# Unit tests
############

# create a class that inherits from unittest
# methods that begin with test_* are run as tests
# has a number of assert methods available
import unittest

# decorators

# This syntax was originally inspired by Java's annotations: just above the definition of the function that is to be decorated, place an at sign (@) followed by the name of the decorator function.

# _functions_ are decorated aka wrapped in the decorating function e.g. memoize


class Ex1:
    # a decorator is a function that takes a function and returns a function
    def some_filter(f):
        return f

    def real_func(self):
        print("hi")

    def do_shit(self):
        decorated_funky = self.some_filter(self.real_func)
        decorated_funky()

    # syntax that does the same as above
    # now when do_shit() gets called, some_filter(do_shit) will be called instead
    @some_filter
    def do_shit(self):
        decorated_funky = self.some_filter(self.real_func)
        decorated_funky()


# built-in decorator functions
#
# 1. @classmethod
#     * indicated the method is a class method
# 1. @staticmethod
#     * indicated the method is a static method
# 2. @property
#     * decorates methods that are get, set or delete property values
