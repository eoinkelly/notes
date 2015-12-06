
# splatting
# =========
a = %w(a b c d)

# *a # => not valid
puts a[0], a[1], a[2]
puts *a # same as above

# rescue
# ======

def foo
  begin
    # fail and raise are aliases
    fail "something bad!"
    raise "something else bad"

  rescue # rescue StandardError

  # these are the same
  rescue => e
  rescue StandardError => e

  # rescue multiple types of exception
  rescue SystemCallError, SignalException, StandardError
  rescue SystemCallError, SignalException, StandardError => e
  end
end

# * You can have multiple rescue clauses
# * Ruby will go through them top->bottom and stop when it finds one that matches
#

# $! ($ERROR_INFO)
# ==============
# * is a thread local global

require 'English'

def func
  fail "something bad happened"
rescue RuntimeError => e
  puts "$!: #{$!}"
  puts "$ERROR_INFO: #{$ERROR_INFO}" # comes from the English module
  puts "e: #{e.message}"
  # raise e # in this case we are giving raise an existing exception instance
ensure
  puts "$!: #{$!}"
  puts "$ERROR_INFO: #{$ERROR_INFO}" # comes from the English module
  puts "goodbye everybody"
end

# Creating a backtrace
#
# * use Kernel#caller
# * format is FILE:LINE:CONTEXT

# * the first element in the array is the lowest-level stack frame
# * the last element is the highest level stack frame
# ??? is this desc correct? the result of caller.first seems to be the most recent frame (top of the stack)
# ??? is my understanding of stacks wrong

caller # back trace not including current line
caller(0) # include the current line

# How exceptions are created
# ==========================

# * Ruby calls the #exception method on the first arg to raise 
# * It does **not** call Exception.new
#     * presumably this is because #exception is easier to override than #new ???
# * this method behaves a bit like a "to_exception" method i.e. an exception coercion method
# * only implemented by Exception and instances of exception objects but you can implement your own

# These have the same result
Exception.exception
Exception.new

# when called with no args it just returns self

e = Exception.exception("a message")
f = e.exception
f.equal? e # => true

# when called with args (a message or backtrace) it returns a duplicate of
# itself (instance variables from the original exception are copied over to the
# new one)
g = e.exception("a new message")
g.equal? e # => false

# How to create your own exception types

class FooThing
  def exception(message="some default")
    RuntimeError.new(message)
  end
end

# how is this different from 

class FooException < Exception
  # how do we set the message in here?
  # ??? what is the syntax for this?
end


# ensure blocks
# =============
# * do NOT explicitly return a value from an ensure block - the returned value
#   will override the exception which will stop the exception from bubbling any
#   farther up the stack

def show_ensure_problem
  begin
    raise Exception, "Bad stuff"
  ensure
    return "all is well"
  end
end

# watch how our exception gets swallowed (it will not be raised any further through the call stack)
show_ensure_problem # => "all is well"

# overriding raise
# ================

# * we can override Kernel#raise and Kernel#fail to have custom behaviour
# * notice that we have to override each one individually so we can potentially
#   have different behaviour for each (this would probably be a bad idea)
module Kernel
  def raise(msg_or_exc, msg=msg_or_exc, trace=caller)
    warn "AH I DIE I DIE: #{msg.to_s}"
    exit!
  end

  def fail(*args)
    raise(*args)
  end
end

# * the Hammertime gem provides an interactive way to recover from exceptions similar to how Smalltalk & Lisp work
#     * I don't see much use for this myself ...
#

# Similarities between when and rescue 
# =================

# * when statements can take a comma separated list of possible values (similar to rescue)
# * they are both language keywords (Kernel#raise is a method)
# * they both use threequals (===) to match against their arguments list
#     * arguments to rescue **must** be classess or modules

foo = "blah"
other_vals = [2, "blah"]

case foo
when 1, "hello"
  puts "1 or hello"
when *other_vals # dynically populated at runtime by splatting the array
  puts "other case"
end

# Creating a retryable error
# ==============

# * return a module that has an #=== method defined on it. 
# * The #=== method just runs the provided block 
# * the return value of the block is the return value of the method
def errors_matching(&block)
  m = Module.new
  (class <<m; self; end).instance_eval do
    define_method(:===, &block)
  end
  m
end


# * An error class that also keeps track of an integer
class RetryableError < StandardError
  attr_reader :num_tries
  def initialize(message, num_tries)
    @num_tries = num_tries
    super("#{message} (##{num_tries}")
  end
end

puts "about to raise"
begin
  # the code in here is responsible for creating a RetryableError with a FixNum that represents how many attempts it has had
  raise RetryableError.new("Some terrible thing", 2) #message, num_tries
rescue errors_matching { |e| e.num_tries <3 } => e
  puts "Ignoring #{e.message}"
end
puts "continuing"


# rescue as statement modifier
# ============================

# * in this case rescue can't take any args except the expression that it should evaluate
# * this means that you can only rescue StandardError (or anything that inherits from StandardError) using statement modifiers

file_or_exception = open("nofile.txt") rescue $!
puts file_or_exception

# in the case above, we either get the file handle (if it was a success) or we get the exeption object
#
# how is this useful ???
#     * you could create an custom exception class that responds to the same messages that your "success object" would (null object pattern)
#     * the exception would have to derive from StandardError
#     * this might be weird?

# Using exceptions as a null object
# ----------------------------------

# CleverError is a sort of null object
class CleverError < StandardError
  def important_thing
    "i did an important thing"
  end
end

def a_risky_thing
  fail CleverError.new
end

thing = a_risky_thing rescue $!

# Comments:
# * thing is either the success object or an exception that behaves like it
# * that seems like a bit of a weird pattern - should i be retruning a proper null object instead instead of glueing stuff onto an exception

# Throwing away exceptions
# ------------------------

# * this just throws away any exception that results from running do_something_risky
# * do_something rescue <expression to evaluate and return>
do_something_risky rescue nil

begin
  begin
    fail "a bad thing"
  rescue
    raise # without args, raise will re-raise $!
    raise $! # same as above
  end
end

# ruby gives us freedom but what are the instances where we have added constraints back in to help us?
# we don't like constraints becuase sometimes they get in our way when we see a solution to our problem but without any constraints we end up inventing the constraints that suit our thinking about the problem. we like constraints but we want to be able to pick them. is that a failing in our thinking, that we can't see an elegant solution within the existing constraints
