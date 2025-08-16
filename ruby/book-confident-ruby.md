# Confident Ruby

## Introduction

Confident code is

- clear uncluttered methods
- methods that tell clear uncluttered stories without getting lost down rabbit
  holes or hung up on tedious type checking.
- sure of itself
- tells the story well
- no provisos or diversions

Timid code is

- constantly second guessing itself
- prone to digressions
- uncertain
- imposes cognitive load on the reader
- mixes up the 4 types of work
- a good story, poorly told
    - tangents
    - digressions

@avdi believes that methods tell their story in this order

1. Collecting input
2. Performing work
3. Delivering output
4. Handling failures
5. Diagnostics (not common)
6. Cleanup (not common)

and that each stanza should not be mixed in with the others.

He compares each method is a _page_ in a story.

## 1. Confident collecting input

- using duck typing - just send the object a message,
- do not use type checks (which includes nil checking, a form of type checking)

#### Aside: Referencing a class (a global constant) in a method is a form of input

```ruby
class Thing
  def initialize
    @foo = Time.now
  end
end
```

`Time` is an input to `Thing.new` just as much as if we wrote it like

```ruby
class Thing
  def initialize(time_klass = Time)
    @foo = time_klass.now
  end
end
```

Use of class constants are "indirect intputs" - we don't care about `Time` so
much as we care about the return value of its `.now`

### Sources of inputs for a method

Mehods have _direct inputs_ and _indirect inputs_

- direct
    - an explicit parameter to the method
- indirect
    - calling another method in the class
    - Any global referenced in the method - includes:
        - global class constants aka classes
        - global variables
    - an instance variable from the current class
    - A constant from the current class

The more indirections that appear in a single method the more coupled that
method is to the code around it.

The more _levels_ of indirection that appear in a single method the more coupled
that method is to the code that is around the code around it!

- => the more likely this method is to have to change if the code around it
  changes
- this is Law of Demeter

### Input collecting (mapping from the objects we have to the roles we want)

- @avdi advocates for thinking about the first stanza of a method as an "input
  collection stanza"
    - maybe separate it from the next stanza with an empty line (depending on
      method length I guess)
- "input gathering" code is inheriently more brittle as it depends on things
  outside of the methods control
    - makes it a good idea to put it in a separate place
    - ?? how far to take this? e.g. is `time = Time; ... time.now` OTT ??
- allows you to map the objects that you have to the roles you want for this
  method (a naming opportunity)
- NB: it is about deciding whether to adapt the method to different types of
  input _or_ to adapt the input to suit the method
    - we want to restrict our checking of inputs to checking that they can play
      the roles we lay down

There are real costs to defensive programming everywhere

- -- it is ugly
- -- takes longer to write
- every method is longer and has at least a stanza more stuff for devs to
  understand
    - -- more cognitive overhead for maintenance
    - -- slower to run at runtime
- we want to "guard the borders not the hinterlands" i.e. It should be done in
  the methods of your app that form the "border" between the app and other
  systems.
- There can be internal borders in a larger app:
    > “In their book Object Design, Rebecca Wirfs-Brock and Alan McKean talk
    > about "object neighborhoods" which together form discrete subsystems.
    > Often, these neighborhoods will interact with the rest of the system
    > through just a few "interfacer" objects.”
    >
    > Excerpt From: Avdi Grimm. “Confident Ruby.” iBooks.

### 3 strategies for getting from the objects we have to the roles we need

1. _Coerce_ objecs into the roles we need
2. _Reject_ unexpected values which cannot play the roles we need
3. _Substitute_ known good objects for unacceptable inputs

It is important to note that the sort of coercing, rejecting, substituing we are
talking about here is definitely NOT appropriate for all methods - see comments
on defensive programming above.

#### coerce using built-in conversion protocols

Ruby has two kinds of conversion methods: _implicit_ and _explicit_. This
distinction comes from how the ruby language and core classes treat the method.

1. Implicit
    - for conversions from a class that is closely related to the target class
    - ruby sometimes sends these messages under the hood to args you provide to
      core and stdlib methods
    - examples:
        - `#to_str`
        - `#to_ary`
    - usage
        - `+` calls `to_str` on args you give it. `Time` instances implement
          `to_s` but not `to_str` so you cannot do `"foo" + now` - you must do
          `"foo" + now.to_s`
2. Explicit
    - ruby will almost never (see the exception below) implicitly send these
      messages to args you provide to core and stdlib methods
    - examples:
        - `#to_s`
        - `#to_a`
        - `#to_h`
    - used to convert between classes that are almost entirely unrelated
    - Ruby core and stdlib methods call `to_str` on pretty much every argument
      they expect to be a string
    - The only case where ruby will automatically call an explicit conversion is
      when it calls `#to_s` during string interpolation. This is inconsistent
      but very convenient.
    - WARNING: be careful when using explicit conversions as they will
      cheerfully convert things like `nil` too.
    ```
    [1] pry(main)> nil.to_s
    => ""
    [2] pry(main)> nil.to_str
    NoMethodError: undefined method `to_str' for nil:NilClass
    ```

Target classes of the conversion also implement the conversion methods so you
can safely call any of them on classes that are already of the target type.

Rubys conversion method provide a subtle approach than type checking - using the
implicit methods is kind of like requiring the argument to implement the "almost
a TargetClass" trait

There is a pattern in the ruby standard lib where methods will call an
"implicit" conversion method on args you give them to ensure the argument can
play the needed role. This means you can pass arbitrary objects as long as your
arbitrary object defines the conversion method.

We can use this same pattern in our own input gathering:

- Anytime a method expects a standard type like String, Array, Hash, Integer you
  should call one of the conversion methods on it to provide added flexibility
  to the caller
    - Use an implicit conversion if you only want objects that are pretty close
      to already being the type to be converted
        - using an implicit conversion says "I want an object that is a Foo or
          has a well defined conversion to a Foo"
        - examples
            - maybe use `#to_int` not `#to_i` if you plan to do important
              calculations with the value
    - Use an explicit conversion if you want to aggressively convert any object
      into the appropriate type

```
things[foo] # calls foo#to_int to convert into an array index
File.open(foo) # will call foo#_to_path to convert into string
```

Conditionally calling conversion methods

- we want to broaden the range of inputs the method will accept
- this does violate duck typing a bit
- avdi prefers to check with a #respond_to? call
- it checks inputs but in a way that is open for extension

```ruby
# filename can be String, Pathname
def foo(filename)
  filename.to_path if filename.respond_to?(:to_path) # call to_path if it supports it
  filename.to_str unless filename.is_a? String # call #to_str if it is not already a string

  # now filename is a string representing a path

  # note that we have not used #to_s because that might convert inputs that
  # don't have a good string representation

end

def foo(filename)
  raise TypeError unless filename.is_a? String
  # very strict
  # does not honor the concept of roles, instead considers the class == role
end
```

You can define your own conversion protocols and use them to convert args

```ruby
def draw_line(start, end)
  # internally we want to work with the points as arrays
  # first we convert them to "coords" role using our protocol method (if they support it)
  start = start.to_coords if start.respond_to? :to_coords
  end = end.to_coords if end.respond_to? :to_coords

  # then we know that "coords" role objects can be sensibly turned into arrays so we do that
  start = start.to_ary
  end = end.to_ary

  # do work ...
end
```

## 2. Confident Performing work

- each message should opeate at the same level of abstraction
- OO design is a high trust environment
- A "captain" object sends messages to subordinates without worrying about
  whether they understand the messages or are qualified to carry out the order.
- The captain does not worry about ship oil pressure

Role

- a single method identifies a single responsibility
- a role is a set of related responsibilities
- note roles != classes
- a role can be played by many objects
- an object can play many roles

To create a "high trust" method you

1. Identify the messages you will want to send
2. Identify the roles that you will send the mssages to
3. Ensure the method "hires" objects that can perform those roles

He shows a design process where

1. write a script for what the method should do in english
1. identify the _messages_ from the english description
1. assign a receiver role to each message
1. turn the table of messages and receivers into code

He acknowledges that this is quite a formalized version of what usually happens.
He emphasises that this process completely ignores the existing roles in the
system. Otherwise:

> “Like a MacGyver solution, the tale becomes more about the tools we happen to
> have lying around than about the mission we first set out to accomplish.”
>
> Excerpt From: Avdi Grimm. “Confident Ruby.” iBooks.

Re-use of code is a good thing but having it as a primary constraint on the
initial design phase of a method is not.

At the initial design phase you want your language to be as close to the problem
domain not the existing solution domain.

We always have _some_ sort of method design process even if we don't think about
it.

- Roles are names for duck types
- duck types don't work if the messages being sent don't match the language of
  the problem space

Common errors with duck types

1. failing to find the right roles(duck types)
2. giving up too early
    - fall back on type checking i.e. `is_a?(Thing)`, `responds_to?(:foo)`,
      `.nil?`

### Aside: My current design process

I currently tend to

1. write a script (or build one in my head) for what the method should do in
   english
    - or have the call points use the method and just stub out the
      implementation
1. convert that to inline code
1. extract message sends to other methods within the same object as sub
   responsibilities become clear
1. give those responsibilities (methods) to other objects in the system if any
   currently exist that fit
    - I think I tend to delay a bit on this - I wonder if I should be more
      agressive about making new objects
1. create new role in the system if I have a significant no. of responsibilities
   that are clearly at a different abstraction level
    - I let the size of objects and methods guide me on when they are getting
      too big and a new role should be pulled out
    - my approach is "do it inline and extract methods and roles until it is
      maintainable"

I guess my thought is

- I want to see the scope of what this method is doing early - expose the
  gotchas
- the simplest solution is probably best
- I like to start with working inline code
    - because it is the simplest to understand
    - but hardest to maintain so then I refactor it into better code
    - seeing the inline code also gives me confidence that I understand the
      task/responsibility now
- pull the code into other roles to help maintainability
    - I see this as a refactoring rather than identifying lots of roles up-front

- what are the pros and cons of my current design process?
    - I don't identify messages and roles up front - i start with inline code
    - I feel like extracting roles is a form of finding abstractions so it is
      better to wait rather than pulling out the wrong role too early
        - ?? this may be a good thing but also may overly constrain my thinking
          to match the shape of the code I have

How could I adjust my process to use tests to drive it?

If I'm making an object for use by existing objects in the system I prefer to do
my first draft of the messages by writing the calls directly into those objects
rather than into tests

I find a unit test a bit artifical for some reason maybe not a good reason? I
want to see the messages I'm adding in the context of the other code in that
caller object

Conclusions

- I think I could improve my OO thinking by thinking more in roles than in
  classes
- I should probably ignore the current roles in the system more than I currently
  do when designing - I think this pushes my designs to be a bit "McGuyver ish"

## 3. Confident Delivering output

## 4. Confident Handling failures
