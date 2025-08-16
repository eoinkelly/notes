# Exceptional Ruby

There is "working right" and "failing right"

A method should present a consistent abstraction in its interface as should a
class. Exceptions are as much a part of this interface as the data types you
return.

## Terms

An _error_ is the presence in the software of some element not satisfying the
specification.

A _failure_ is the inability of a software element to satisfy its purpose.

An _exception_ is the occurance of an abnormal condition during the execution of
the sofware.

Errors cause failures which in turn cause exceptions

## What does it mean for a method to "fail"

- All methods have a contract (explicit or implicit) with the caller:
    - "given the following inputs I promise to return a certain value or have
      certain side effects"
    - it is the caller's responsibility to make sure the method's
      "preconditions" are met
        - i.e. the caller is responsible for "setting up the world"
        - => a method has the right to be invoked in a valid world
    - the method is also responsibile for maintaining the "invariant" (note:
      singular) of the object it is part of
        - the "invariant" is the _set_ of conditions that must be true for the
          object to be in a consistent state e.g. traffic light object with only
          1 bulb lighting at a time.
    - it is the methods responsibility to make sure the "post-conditions" are
      met
        - post-conditions = outputs + side effects
- A method can be said to have "failed" when it does not fulfil this contract
    - A method has failed when its preconditions are met and it does not
      maintain the invariant or its postconditions
        - => it is not the methods fault if it cannot function when the
          preconditions are not met - that is the fault of the caller

Errors can be:

1. an error in implementing the method (i.e. the idea is good but the
   implementation is not)
1. not understanding how ruby works
1. not understanding how some third party lib works
1. not understanding how the operating system works
1. not understanding how some other service (e.g. API) works

```
raise <exception class>, <message>, <stacktrace>
fail <exception class>, <message>, <stacktrace>
```

- exceptions can be raised from C code via `rb_raise()` C function
- raise and fail are just methods on Kernel - you can override them
    - overriding only works for exceptions raised from Ruby - the `rb_raise`
      will call straight through to the default implementation.
- You can pass a stacktrace as an arg
    - handy for test suites which don't want to include their own methods in the
      stacktrace
    - a common thing to pass is `caller`
        - `Kernel#caller` without any args returns stacktrace up to current
          stack frame but not including current stack frame

```ruby
# Kernel#raise
# Kernel#fail

module CustomRaiseAndFail
  def raise(exc_or_msg, msg=exc_or_msg, trace=caller)
    # do custom stuff
    # QUESTION: do I just call super here?
  end

  def fail(exc_or_msg, msg=exc_or_msg, trace=caller)
    # do custom stuff
  end
end

module Kernel
  include CustomRaiseAndFail
end
```

```ruby
# Exception.exception
Exception.exception(msg) # does same as Exception.new(msg)

Exception#exception
some_exc.exception # no args, returns self
some_exc.exception(other_msg) # duplicates some_exc and sets new message
```

```
# Note that because raise calls #exception not #new we can delegate the
# exception to be raised to another object

class SomeHttpResponse
  def exception(msg, stacktrace=caller)
    RuntimeError.new(msg, stacktrace)
  end
end

# ...
result = get_response('http://blah')
if result.status > 400
  raise result
end
```

Consider

```
raise FooError, "bad thing"
```

This is what it does:

1. Call Exception#exception to get the exception object
    - raise does not call FooError.new(...) as you might expect
    - it calls `foo_err = FooError.exception("bad thing")`
1. Set the backtrace by calling foo_err.set_backtrace(caller(0))
1. Set the global exception variable $! ($ERROR_INFO)
1. Throw the exception object up the call stack
    - ruby works its way up the callstack looking for `rescue` and `ensure`
      blocks

### ensure

- an ensure clause is _always_ executed whether an exception is raised or not
- use them for cleaning up resources e.g. file handles
- try very hard to not have code in your ensure block raise an exception as that
  means some of the block will not be executed which might leave your resources
  dangling.
- WARNING:
    - explicit return from within an `ensure` block is an anti-pattern as it
      will take rprecedence over the exception being raised so effectively
      throws away the exception.

### retry

- ruby has built-in syntax for retrying a block that raised an exception

```ruby
# simple code to retry a block of code 3 times
tries = 0
begin
  tries += 1
  # ...
rescue FooError
  retry if tries < 3
end
```

### raise

```ruby
begin
  # ...
rescue FooError => e
  raise e # just re-raise the exception
  raise NewError # raise a new exception (old one will be in #cause)
end
```

### rescue block

- a short sequence of simple instructions designed to
    1. bring the object back into a stable state
    1. retry the operation or end with a failure
- rescue will evaluate its args as an expression so you can dynamically generate
  them
    - Caveat: All its args must be classes or modules
- rescue can be used as a statement modifier `do_risky(thing) rescue BadThing
- ruby matches exception classes in a `rescue` block using `===` (same as
  `case`)
    - unlike case it requires that you only pass a class or module

```ruby
begin
  # ...
rescue => e # same as: rescue StandardError => e
  # ...
end


interesting_errors = [FooError, BarError]
rescue => *interesting_errors
```

### else (the opposite of rescue)

- only hit if an exception is _not_ raised
- exceptions raised within an else block are NOT captured by the preceeding
  rescue clauses - instead the propagate up to the next stack frame

### exit hooks

- if an exception is not handled anywhere it will be printed out to console by
  ruby and the program will exit.
- but on the way towards the exit ruby will run its three exit hooks
- the exit hooks are run in the following order:

```ruby
trap('EXIT') { puts 'trapping EXIT' } # 1.
at_exit { puts 'in at_exit' }         # 2. Kernel#at_exit
END { puts 'in END' }                 # 3.
```

- all exit hooks have access to $ERROR_INFO so can work with it

### Exceptions and threads

- an exception raised in a thread ends the thread and the exception is re-raised
  in the main thread when the dead thread re-joins it

### $ERROR_INFO $!

- each _thread_ gets its own copy of this special "global"
- It is always nil unless an exception is currently being raised

### caller and caller(0)

- caller returns the stacktrack not including the current frame
- caller(0) includes the current frame

### Stack traces

Stack traces in ruby are just an array of strings

### Nested exceptions

- Since ruby 2.1
- use the Nestegg gem if using an older ruby

## Exceptions speed

- They are not super quick in ruby
- Tldr is don't use them for flow control - they should be sad path only

# 7 ways to respond to failure

There are at least seven common ways to respond to failure:

1. return a failure flag
    - `nil` is a common but not great choice
2. return a benign value
    - if the systems success does not depend on the outcome of the method then
      this can be a gentle way to deal with failure
    - e.g. JSON parser returns Hash with expected keys but empty strings for
      values
    - can be a good alternative to returning nil
3. Report error to console
    - ruby has `warn` method that is like puts for warnings
4. Report error to logs or remote server
5. Use a bulkhead (like a ship does)
    - allows the ship to be sealed internally so if one part takes on water the
      bulkhead for that area can be sealed and prevent water getting everywhere.
    - enforce a principle of damage limitation
    - see "Release it!" book
    - avdi recommends putting them between your app and
        - external processes
        - external services as these are a common source of unexpected error.
    - bulkhead tactics in ruby
        - `rescue Exception` then log it and maybe re-raise it
6. Use a circuit breaker pattern
    - described in "Release It!" book
    - prevents cascading failures
    - mechanism has three states
        1. closed
            - subsystem operates normally
            - counter tracks no. of errors
            - circuit breaker trips if no. of errors (or error rate) gets too
              high. Then circuit breaker enters open state.
        1. open
            - subsystem cannot operate
            - then after human intervention or a timeout or some other criteria
              the system enters "half-open" state.
        1. half-open
            - subsystem can operate but a single failure will send it back to
              "open" state
            - subsystem is on probation
7. end the program
    - some failures mean you cannot continue
    - ruby has multiple ways to end a program

### 3 Ways to end a ruby program

1. `exit('message')`
2. `abort('message')`
3. `exit!(integer)`

```ruby
puts 'hangin out, just being a program'

# use `echo $? after running this script to check its return value in the shell

trap('EXIT') { puts 'trapping EXIT' } # 1.
at_exit { puts 'in at_exit' }         # 2. Kernel#at_exit
END { puts 'in END' }                 # 3.

# stops execution at this line. ruby will not find any code after this line so
# make sure exit hooks are defined above it
abort 'blast'

# abort('blast') is the same as:
warn 'blast'
exit(1)

# exit! will skip all exit hooks and "finalisers" (TODO: what are they?)
exit! # same as exit!(1)
exit!(3) # exits with 1
```

In all cases try to think about what happens if your failure reporting has an
error - will it casue a cascade of errors? Example: if raygun went down does our
code throw an exception if it can't reach it? Would those exceptions crash a
worker?

> “because systems are typically built with most of the attention paid to the
> steady-state—and because failure conditions are often hard to
> simulate—failure-handling subsystems are often some of the most fragile and
> untested in the project”
>
> Excerpt From: Avdi Grimm. “Exceptional Ruby.” iBooks.

# 8 alternatives to exceptions

A methods primary channel of communication with its caller is its return value.

If we want to communicate failures without exceptions we need another channel -
one that can pass "meta information" about the status of the method (or process
that it represents) i.e. we need a "sideband" channel

1. multiple return values
    - -- caller has to expect it or they get a surprise array
    - ++ ruby has pretty good support for this with splat/sponge operator
        ```ruby
        status, headers, response = get_http_request('/blah')
        ```
    - -- adds an ordering depencency that returning a simple structure will
      avoid in the same way that named parameters do
2. return a simple structure (e.g. a hash or OpenStruct) that allows for
   metadata
    - mimics tuples in other languages
        - those languages have pattern matching to make branching based on a
          some "status flag" easy
    - e.g. `OpenStruct(status: :ok, value: ...)`
    - ++ the structure you return can have some nice predicate methods defined
        - -- but this again turns it into a new "type"
    - ?? is a hash mentally easier to deal with rather than a new simple type ??
    - -- caller has to expect it
    - ++ the contract around the return value is more explicit than returning an
      array
    - compared to caller supplied output params
        - caller has to expect it in both
        - caller code is harder to read if it supplies output parameters
3. caller supplied output parameter(s)
    - like C stdlib functions
    - pass in an empty arguments that the method can mutate
    - pass in as many args as you need to create "sidebands"
    - ++ as in all things C it errs on the side of less allocations
    - -- more or less the same result as returning a simple structure but less
      elegant
        - ?? maybe less allocations ??
    - caller can then inspect this arg to decide how things went
    - handy for logging
    - note that `StringIO.new` as a default value is handy

        ```ruby
        def do_thing(a, b, transcript=StringIO.new)
        transcript.puts "doing stuff"
        do_stuff
        end

        tr = StringIO.new
        do_thing(4, 2, tr)
        # now tr contains the full transacript of what happened
        puts tr
        ```

4. caller supplied fallback strategy
    - inject a failure policy as an argument (either block or method reference)
        - `method(:my_fail_pol)` generates a reference to a method
    ```ruby
    def do_thing(a, b, failure_policy=method(:raise))
      # ...
      failure_policy.call("some value")
    end
    ```
5. Global (or at least Thread local) variables
    - like C stdlib does it
    - -- we have to be careful to reset the variable
    - -- global state is not without its problems ...
6. Process reification
    - represent the process as an object
    - give the object some attributes to store metadata e.g. status,
      success/fail
    - ++ clean OO solution
    - -- kinda heavy as you have a new class (aka type) in the system that must
      be understood
    - example: a service object that has a `#perform` method and a `#problems`
      attr_reader
7. Cond lib
    - taken from common lisp condition system
    - -- relies on ruby `raise` - will not work with exceptions raised by C
      code.
    - http://quix.github.io/cond/
    - seems mildly abandoned (last tested on ruby 1.9.2)
    - SOMEDAY: dig a bit deeper into this pattern, probably in context of
      learning common lisp
    - http://www.gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts.html
8. Monadic errors
    - Swift, Haskell do this
    - ruby libs
        - https://github.com/pzol/deterministic
        - https://github.com/avdi/naught/
        - https://github.com/tomstuart/monads

# Failure handling strategy grab-bag

- Exceptions should not be expected
    - they are often overused
    - don't use an exception everytime you get an answer you don't like. Instead
      do ???
    - they distort the flow of control
    - they can lead to overengineering
    - there is no such thing as an event that "always" throws an exception - it
      is always contextual!
    - examples
        - fail to open a file
            - couldn't really be described as totally "unexpected"
        - fail to get a successful response from another HTTP service
            - again there is a fairly high likelyhood of failure
        - ActiveRecord #save does not raise an exception as failing to save a
          record to the database is not "totally unexpected" as it might depend
          on user input.
            - OTOH if your user input has been through validation then raising
              an exception is more unexpected.
            - AR provides #save and #save! alternatives to allow the caller to
              choose whether they want an exception or not
    - rule of thumb from the pragmatic programmer
        - "Will this code still run if I remove all the exception handlers? If
          no then maybe exceptions are being overused"
- Ruby's `throw` and `catch` can be used instead of exceptions for dealing with
  things which might fail but don't warrant an exception. This lets you save the
  concept of "exception" for truly exceptional cases in your app.
    - throw cna throw a value up to the nearest lexical catch
    - -- throw and catch distort flow control just as much as exceptions do
- The context matters when deciding whether something should throw an exception
  or not e.g.
    - database save without validation of user input => fairly expected => don't
      raise exception
    - databse save after validation => much less expected => can raise exception
- When you raise an exception you force the caller to treat the condition as an
  exceptional case whether it argrees with you or not
    - e.g. if you get a HTTP 4xx response and throw an exception then the caller
      is forced to deal with 4xx responses in a differnt way to 2xx or 3xx which
      it may not want to do.
- Caller supplied fallback strategy is a really useful pattern
    - Practice of programming book: "In most cases the caller should determine
      how to handle an error not the callee"
    - e.g.
        - Hash#fetch lets you provide a default value or block to run if the key
          cannot be found. This lets the caller define the policy for missing
          keys.
        - Enumerable#detect was already taking a block so can take a lambda that
          is run if no values are detected

Five questions to ask before raising an exception

1. Is the situation unexpected enough?
2. Am I prepared to end the program?
3. Can I punt the decision up the call chain?
    - maybe capture and log failure and then delegate to some code provided by
      the caller
4. Am I throwing away valuable diagnostics?
    - less of an issue with nested exceptions
    - another way of looking at this is "Do I have any important info to add to
      this error?"
5. Would continuing result in a less informative error?
    - Sometimes it is better to raise earlier than later as you might have lost
      important details about what was being attempted.

“An exception represents an immediate, non-local transfer of control—it's a kind
of cascading goto. Programs that use exceptions as part of their normal
processing suffer from all the readability and maintainability problems of
classic spaghetti code.

– Dave Thomas and Andy Hunt, The Pragmatic Programmer”

Excerpt From: Avdi Grimm. “Exceptional Ruby.” iBooks.

- Nesting begin...rescue blocks is a bad idea - makes code very hard to follow
- @avdi considers `begin` to be a code smell and I think I agree

### Contingency method pattern

```ruby

# If you have a bunch of methods that all need to rescue the same exceptions
# AND do the same thing when rescuing then this pattern is neat

def with_http_error_handling
  yield
rescue HttpError => e
  # handle the error
end

with_http_error_handling {
  # body of your method 1
}

with_http_error_handling {
  # body of your method 2
}
```

### Aside: consistency vs validity

- valid object:
    - all the business rules are met
- consistent object:
    - a subset of valid
    - the object can operate without exhibiting undefined or unexpected
      behaviour
    - the object invariant contains rules to maintain its consistency - the
      invariant does not specify that the object has to be "valid"
- examples
    - ActiveRecord object can be valid or not but is always consistent
      (hopefully) i.e. no dangling links to object that no longer exist in
      memory

## The three levels of exception guarantee

- Critical methods need exception guarantees!
- Classically there are three levels of exceptions that define what the internal
  state of the object will be when an exception is raised:
    1. Weak guarantee
        - If an exception is raised the object will be left in a consistent
          state
    2. Strong guarantee (rollback guarantee)
        - If an exception is raised the object will be rolled back to its
          beginning state
    3. Nothrow guarantee
        - this method will never raise an exception - all errors will be handled
          internally
        - in reality it is pretty much impossible to guarantee this!
        - There is no such thing as code that can _never_ raise an exception!!!
        - An exception can happen at _any_ point in your code!
            - NoMemoryError will happen if ruby fails to allocate
            - SignalException will happen if the OS sends the ruby process a
              signal
- There is also an implicit 4th level which is "this method makes no guarantee
  at all"

Avdi has an ["exception tester" script](https://gist.github.com/avdi/772356)
that runs a method once in "record mode" and logs all of the method calls it
makes. It then runs the method in "playback mode" causing each call to raise an
exception in turn and running some consistency check (defined by you) on the
object after it runs. It is a very interesting idea - TODO: dig in more to this

# The impact of exceptions on design

- _Any_ method call can fail at _any_ time!
- so you pick a certain level of "unexpectedness" and raise exceptions above
  that
- exceptions do push your design in certain ways
    - it stops callers from deciding what constitutes "red alert" errors and
      less important error conditions
- exceptions force the callee's views on what is a "red alert" ("thou shalt not
  treat this like any other error, I demand you make an exception!!!) problem on
  the callers
- exceptions make error handling more rigid

# Tag Exception instances with a module pattern

- rescue can match module names as well as class names
- you can create empty modules and extend your exceptions to include them - this
  makes the module names act as tags that rescue can match against.

```ruby
require 'some_dodgy_lib'

module Errors::DodgyLib; end

def things
  # intro stuff
  begin
    do_dodgy_thing
  rescue Exception => e
    # In here I know that whatever it was it came from dodgy_lib
    e.extend(Errors::DodgyLib)
    # re-raise the (now tagged) exception
    raise e
  end
  # outro stuff
rescue Errors::DodgyLib => e
  # stuff
end
```

# How to decide what exception herirarchy I need

can organise based on

- severity
- module
- layer

As of 2011 @avdi is tending to using:

- UserError
- LogicError
    - InternalError
    - ClientError (for libs)
- TransientError
    - TimeoutError

based on working backwards from where the errors will end up.
