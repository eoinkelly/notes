
# What is the best way to create a custom exception in Ruby
# ====================

class CustomError < StandardError
end

class CustomError < StandardError
  def initialize(msg="", some_var)
    super(msg)
    @some_custom_var = some_var
  end
end

# Notes:
# * we want to inherit from StandardError so that rescue without params will check us for a match

# Demonstrate all the syntax available to ruby exceptions
# ============================
begin
  fail "A bad thing happened" # Kernel#fail and Kernel#raise create a RuntimeError by default
  # raise "A bad thing happened" # does same as line above
rescue => e
rescue StandardError => e
else
  puts "I am run if no exception is raised from the begin block"
ensure
  puts "I am always run"
  # do not have an explicit retrun value from  here - it causes the currently active exception to be thrown away
end

# Demonstrate how exception handling can be added directly to a method
# =====================

def foo
  # do risky stuff
rescue CustomError => err
  # handle a custom error
rescue IOError, StandardError => err
  # handle errors
else
  # handle the success case
ensure
  # do any cleanup
end

# * @avdi recommends not using nested begin blocks as it gets very confusing -
#   he recommends the style above where a single method has it's exceptions added
#   to it

# What is the best way to retry a risky operation X times in ruby
# ====================

# retry will resteart from the last enclosing begin block

# option 1 (@avdi)
# ----------------
tries = 0

begin
  tries += 1
  puts "Attempt #{tries}"
  # do risky stuff
  fail "I failed"
rescue
  retry if tries < 3 # will restart execution from the start of the enclosing begin block

  # handle the case where we give up 
  puts "gave up"
end
# * I initially used a custom error here that had a @tries attribute but it didn't avoid needing the global counter
# * 'tries' is not very elegant - any way to avoid it???


# option 2 (pickaxe)
# ----------------
class RetryException <  StandardError
  attr :ok_to_retry

  def initialize(ok_to_retry: false)
    @ok_to_retry = ok_to_retry
  end
end

tries = 0
begin
  tries += 1
  # risky stuff
  fail RetryException.new(ok_to_retry: (tries < 3 ? true : false))

rescue RetryException => e
  retry if e.ok_to_retry
  puts "giving up"
end
# * this doesn't get rid of the tries global


# option 3
# ========
# could have a custom exception that when it was created, registers itself with
# some other object that keeps track of retries. then that object becomes a glorfied version of the tries object (which is the simplest implementation of such an object)



# What is the cleanest way to run risky code and capture the exception (if any)
# =============================================================================

def do_risky_thing
  fail "bad thing"
end

# option 1
# --------
foo = do_risky_thing rescue $! # foo is either the return value or the currently active exception (not sure how this is useful)
if foo.kind_of? Exception # #kind_of? checks the whole ancestors chain. #instance_of? only checks the first element of the ancestors chain
  # handle the error
else
  # handle the success
end


# option 2
# --------
begin
  foo = do_risky_thing
  # handle the success
rescue Exception => e
  # handle the error
end

# What is the best way to run some code and ignore some (but not all) exceptions
# ============================================================================


# option 1 (from @avdi)
# ---------------------

def ignore_exceptions(*exceptions)
  yield
rescue *exceptions => e
  puts "Ignoring #{e}"
end

ignore_exceptions(IOError, SystemCallError) do
  open("not.exists")
end

# option 2
# --------

begin
  open("not.exists")
rescue Exception => e # rescue all exceptions
  if e.instance_of?(IOError) || e.instance_of?(SystemCallError)
    puts "Ignoring #{e}"
  else
    # re-raise the exception if is not on our ignore list
    raise e
  end
end

# option 3
# --------
begin
  open("not.exists")
rescue IOError, SystemCallError => e
  puts "Ignoring #{e}"
rescue Exception => e # catch-all
  raise e
end

# What is the best way to run some code and ignore all exceptions
# ===============

# option 1
# --------

do_risky_thing rescue nil

# option 2
# --------

begin
  do_risky_thing
rescue Exception
  # rescue all exceptions but don't do anything
end


# It is easy to rescue exceptions based on their type (class) but what is the
# best way to rescue exceptions whose other attributes (e.g. name, message)
# match a particular pattern?
# ====================

# option 1 (@avdi)
# ----------------

# The code:
#   rescue foo => e
# calls foo.===(e) if foo.=== returns true, it is considered a match

exp_name_match = Object.new  # create a new generic object
def exp_name_match.===(e)    # add the #=== method to it's singleton class
  /^A/ =~ e.name
end

begin
  do_risky_thing
rescue exp_name_match => e # rescue calls exp_name_match.===(e)
  puts "We are handling the exception whose name matches our regular expression"
end

exp_msg_match = Object.new
def exp_msg_match.===
  /^A/ =~ e.message
end

begin
  do_risky_thing
rescue exp_msg_match => e # rescue calls exp_msg_match.===(e)
  puts "We are handling the exception whose name matches our regular expression"
end


# option 2 (@avdi)
# --------------

def do_risky_thing
  fail "bad thing"
end

def errors_matching(&block)
  m = Module.new
  (class <<m; self; end).instance_eval do
    define_method(:===, &block)
  end
  m
end

begin
  do_risky_thing
rescue errors_matching { |e| e.message =~ /^Hello/ } => ex
  puts "#{ex} has a message that matches"
end

# Questions
# * are there other ways of taking a block and turning it into a method on an object?


# What is the best way to create a nested exception in ruby?
# =================
#
#

# option 1 (ruby 2.0 and lower)
# -----------------

class NestingException
  attr_reader :original

  def initialize(msg, original = $!)
    super(msg)
    @original = original
  end
end
# * notice the intelligent defaulting of the original exception to $! (the currently active exception)

# option 2 (nestegg gem)
# ---------------
# * the NestEgg Gem implements a more fancy version of this

# option 3 (ruby 2.1 and above)
# -------------------------
# * Exception#cause
# * you cannot set it explicitly (this makes it less flexible than our manual approach)
# * the contents of $! are automatically put in it when you raise an exception

begin
  begin
    raise "Error A"
  rescue => error
    raise "Error B"
  end
rescue => error
  puts "Current failure: #{error.inspect}"
  puts "Original failure:  #{error.cause.inspect}"
end

# Current failure: #<RuntimeError: Error B>
# Original failure:  #<RuntimeError: Error A>

# here we don't catch the exception to see what kind of stack trace ruby gives
begin
  begin
    raise "Error A"
  rescue => error
    raise "Error B"
  end
end
# => RuntimeError: Error B
# * not terribly informative ...


# What is the best way to tweak an exceptoin before re-raising it
# ==================


# option 1 (ruby 2.1+)
# --------------------
begin
  begin
    begin
      fail "the cause" # fail RuntimeError, "bad thing"
    rescue => e
      puts "============"
      puts "e: #{e}"
      puts "$!: #{$!}"
      fail "bad thing" # the #cause of this exception will point at the original "the cause" exception
    end
  rescue => e
      puts "============"
    puts "e: #{e}"
    puts "e.cause: #{e.cause}"
    puts "$!: #{$!}"
    raise e, "new message" # this duplicates the exception and gives it a new message. This means that it's cause is still the same
  end
rescue => e
      puts "============"
    puts "e: #{e}"
    puts "e.cause: #{e.cause}"
    puts "e.cause.cause: #{e.cause.cause}"
    puts "$!: #{$!}"
end
# * notice how when we re-raised the exception it kept it's original cause attribute
# * the bottom line is that you can change the message in an exception before you
#   re-raise it (it will be a new object but the other attributes will be copied
#   over from the old one


# What are the intesting messages you can send an instance of a ruby exception?
# ==================

# There are 6 methods on StandardError that are not inherited from Object
e = Exception.exception
se = StandardError.exception

# > se.methods - Object.methods
#  :exception,
#  :message,
#  :backtrace,
#  :backtrace_locations,
#  :set_backtrace,
#  :cause


# How can we prevent ruby from allowing double raising exceptions like $lang
# =========================

module NoDoubleRaise
  def error_handled!
    $! = nil
  end
  def raise(*args) # Kernel#raise is a method that can be overriden
    if $! and args.first != $!
      warn "Double raise at #{caller.first}, aborting" # warn prints to STDERR not STDOUT
      exit! false # exit immediately, do not run any exit handlers, and return false to the parent process
    else
      super
    end
  end
end

class Object
  include NoDoubleRaise
end

# break out of our new rule
begin
  raise "initial failure"
rescue
  error_handled! # this allows us to raise a new exception
  raise "Secondary failure"
end

# How can you recover from a system exit call in ruby
# ================================

status_code = true 
# generally: true for success, false for failure, other values are system dependant

Process.exit status_code # raise SystemExit

begin
  Process.exit status_code # raise SystemExit
rescue SystemExit
  puts "You can never leave"
end
  
# What are ruby's exit handlers
# ==========================

# 1. Kernel#at_exit
# 2. ObjectSpace.define_finalizer("string", proc { puts "hi from finalizer" })

Kernel.at_exit { puts "1" }
at_exit { puts "2" }
ObjectSpace.define_finalizer("string", proc { puts "3" })
exit

# returns
# 2
# 1
# 3

# notes:
# * the at_exit handlers seem to be invoked in the opposite order than they were defined


# What are the use-cases for rescue ... else syntax
# ======================

# exceptions raised in an else block are *not* captured by the preceeding rescue clauses. Instead they are propagated up to the next higher scope of execution. This makes them very handy for running risky-code-B that should only run if risky-code-A was successful

# Consider this anti-pattern
def risky_a
  fail "some A way"
end

def risky_b
  fail "some B way"
end

def ignore_errors
  risky_a
  risky_b
rescue Exception => e
  puts "Error running code: #{e}"
end

# * the exception raised by risky_b will cause the exception raised by risky_a to be ignored so you will never find out that risky_a was failing too

def ignore_errors
  risky_a
rescue Exception => e
  puts "Error running code: #{e}"
else
  risky_b
end

# * this version will rescue the exception from risky_a but will propagate the one from risky_b up the stack


# is there a way to write this to handle them both
# ===================

# option 1
# --------

def ignore_errors
  risky_a
rescue Exception => e
  puts "Error running code: #{e}"
  # do stuff to handle it
else
  begin
    risky_b
  rescue Exception => e
    # do stuff to handle the exception
  end
end

# * this is pretty clunky


# option 2
# --------

def ignore_errors
  ignore_a_errors
  ignore_b_errors
end

def ignore_a_errors
  risky_a
rescue Exception => e
  # do stuff to handle it
end

def ignore_b_errors
  risky_b
rescue Exception => e
  # do stuff to handle the exception
end

# * this is cleaner, one exception rescue per method
