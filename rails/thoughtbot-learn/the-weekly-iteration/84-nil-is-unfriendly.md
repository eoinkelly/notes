## Weekly Iteration 1: Nil is unfriendly

### Aside: law of demeter

law of demerter violation is you making structural duplication

`user.account.subscription.plan.price`

- you declared these relationships in the models (first use)
- now you are repeating that structure in whatever code is running the line
  above (second use)
- both these places will have to be changed if you change the structure
- and it tends to breed other places where you will do this (because you have no
  good alternative in place)

how do I mitigate a LoD violation?

```ruby

class User
  def price
    account.subscription.plan.price
  end
end

# user.account.subscription.plan.price # old way
user.price # new way

```

- does the above really solve it?

### nil breaks duck typing

`user.account.subscription.plan.price`

- problem: 1: you have to manage nils at all points in this chain
- problem 2: nil has vague meaning:
    - is there no price?
    - is the price zero?
    - is this a bug?

- "every time you use nil, you are violating duck typing". why?
    - duck typing says that we shouuld be able to send the same message to
      different types of things and not care as long as they implement the
      message.
    - say you have a situation weith 3 objects that all implement the same
      method (quack the same) and you send a messag to them - if any of those
      ojbect got set to nil this will fail because nil doesn't implement _any_
      methods so will never quack like anything!

using nil is similar to using #is_a to check the class of a bunch of things
using nil forces us to insert a conditional into our code to handle it (even if
we use a sneaky one like #try) it makes us handle the "nil case" at many points
in the program - hello duplication!

```ruby
# how do we handle situations where this thing might returns nil?
subscription.price

# this version runs #price and if it is nil, it evaluates to false which means that the expression returns 0
subscription.try(:price) || 0

```

### 3 Alternatives to nil

How you replace nil depends on what kind of situation you have

1. you would always do the same thing if you got a nil
2. nil is a genuinely unexpected situation (a bug)
3. Q: are there other situations? \* e.g. "i _sometimes_ expect a nil" 3a. if
   fits into neither of the above two cases. Here we can at least use Maybe
   pattern to stop us forgetting to check for it.

4. null object pattern
    - useful for anytime you have a nil you know how to hande it
    - they argue that calling these thigns Null* is bad as it is not discriptive
      e.g. NullUser should be Guest. NullUser doesn't tell you *why\* there is
      no user e.g. was it a bug?
    - null objects tend to encapsulate conditional logic, not remove it
    - null objects are _not_ a good fit if you can't roll the handling of the
      conditional into it. if you use a null object in this case you'll just end
      up testing the class name at some stage which is no win

5. raise an exception

- useful when we know that nil represents an unexpected outcome - better to fail
  loudly and early
- use this when nil is _always_ a bug
- use it when nil needs to be handled very far away so raise an exception

3a. Maybe pattern

- forces you to use conditional logic
- is a pattern for replacing nil
- helps you avoid invalid scenarios because it forces you to check whether you
  got something or nothing
- you will still have repeated conditionals around your program checking for
  whether it is something|nothing but mabye does prevent you from forgetting
  this check (becaue you have to unwrap the object before you can use it)

```ruby
# a Maybe pattern implementation in ruby
# see the Wrapped gem for more info

class Some
  def initialize(object)
    @object = object
  end

  def map # takes a block
    new_ob = yield @object # new_ob is the _return value_ of the block

    # create a new instance of some that wraps whatever the block returns
    Some.new new_ob

    # or the short version ...
    # Some.new yield(@object)
  end

  def unwrap_or(default)
    @object
  end
end

class None
  def map # map does nothing
    self
  end

  def unwrap_or(default)
    default
  end
end

# foo
# * returns a "Maybe thing"
# * but ruby doesn't force any particular type on the thing that is returned
def foo
  if it_worked
    return Some.new "some value"
  else
    return None.new
  end
end

```

### Open questions:

if haskell's pattern matching multiple function bodies is so great why don't we
have methods that contain a big switch statement in ruby? does the nice syntax
make it a good idea?

is the reason that maybe pattern feels weird in ruby because ruby doesn't have
pattern matching? haskell has a lot of built-in support for doing a #kind_of?
comparison but it is frowned upon in ruby and we have to do explicit wrapping
and unwrapping
