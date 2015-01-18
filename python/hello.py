#!/usr/bin/env python3

"""I am the doc string for the module apparently"""

def show_prints():
    """docstring for show_prints"""

    val = 34

    print("Hi there")
    print('Hi there')
    # print      ('Hi there') # legal but bad form
    print("hi", "there")
    print("val is %s" % val) # funky syntax for string formatting
    # print 'Hi there' # print is statement in 2.x, breaks in 3.x

show_prints()

# show_type_coercion() # does not work as functions much be defined before being called

# it does not seem to do type coercion
def show_type_coercion():
    """docstring for show_type_coercion"""
    better_foo = 13
    # print('foo is ' + foo) # fails
    print('foo is ' + str(better_foo)) # fails

show_type_coercion()


def play_with_strings():
    """docstring for strings"""
# seems like . calls a method of an object
# also seems like comment indenting doesn't break out of functions
    exp = "example {0}".format(10)
    print(exp)

play_with_strings()
