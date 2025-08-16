## Weekly Iternation 2: Types of coupling

- coupling = the _degree_ to which components in a system rely on each other.
    - Some coupling is unavoidable but we should be
        1. recognise the different types of coupling in code
        2. rank the different types of coupling so we can understand the
           trade-offs

- coupling and cohesion
    - cohesion = ???
    - low coupling often correlates with high cohesion

TODO: read up and think about on coupling
http://en.wikipedia.org/wiki/Coupling_(computer_programming)
http://robots.thoughtbot.com/types-of-coupling

- connaissaince ~= changes in B have a high likelyhood of requiring changes in A
    - TODO: read up on connaissance

- the system needs to have some coupling

three different types of coupling:

1. Pathological Coupling,
    - e.g. reaching across the encapsulation barrier with #instance_variable_set
    - monkey patching falls into this category

2. Global Coupling
    - e.g. factory girl basically has a singleton factory that creates Users
      which means that I can't easily change that factory because so much
      depends on it. FactoryGirl uses a global repository for factories

3. Control Coupling.

```ruby
# every client of foo will have to change if you change hw do_validations_flag works
# clients of #foo are coupled to the internals of #foo
# * this is an example of a parameter that is a control couple
def foo(do_valiations_flag)
  if do_validations_flag
    do_validations
    save
  else
    save
  end
end

foo(false) # <- client code knows exactly how you manage not saving validations internally
```

4. Data coupling

```ruby
class ScreenPrinter
  def print(text)
    output_to_screen(text.to_s)
  end
end
# * what happens if text does not have a #to_s method?
# * client code has to know that it must pass a param to #print that has a #to_s - this
#   means that some of the knowledge of how #print works has leaked into the client code.
# * this gets worse the more params you pass - this makes methods that take 0 args better than those that take any
# * it also gets worse the more method in ScreenPrinter that take 'text'
```

5. Message coupling

```ruby
# This is "low coupling" - we are just coupled on the name of the message i.e.
# client code only knows the name of the message to send
# * You can't/shouldn't always do this
class ScreenPrinter
  def print
    output_to_screen(@text.to_s)
  end
end

# When considering two design decisions perfer the one that has lower coupling
```

When you feel tempted to pass in a boolean flag to a method it is a smell
especially if you then use that in a conditional.

classes are objects in ruby and their public API is inheritance if you monkey
patch a class you are not using its public API, you are reaching right in there
(similar to using #instance_variable_set on object instances)
