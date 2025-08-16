# super

- super is a _keyword_ in ruby NOT a method call!
    - even though it looks like a method call in some instances
- Called from a method, searches along the method lookup path (the classes and
  modules available to the current object) for the next method of the same name
  as the one being executed.
- Such method, if present, may be defined in the superclass of the object‘s
  class, but may also be defined in the superclass‘s superclass or any class on
  the upward path, as well as any module mixed in to any of those classes.

```ruby
module Vehicular
  def move_forward(n)
    @position += n
  end
end

class Vehicle
  include Vehicular  # Adds Vehicular to the lookup path
end

class Car < Vehicle
  def move_forward(n)
    puts "Vrooom!"
    super            # Calls Vehicular#move_forward
  end
end
```

- Called with no arguments and no empty argument list, super calls the
  appropriate method with the same arguments, and the same code block, as those
  used to call the current method.
- Called with an argument list or arguments, it calls the appropriate methods
  with exactly the specified arguments (including none, in the case of an empty
  argument list indicated by empty parentheses).

When you call super as a bare word, it passes the args from the enclosing method
automatically, to force it to pass no args you need to use super() .

## Gotcha super vs super()

```ruby
super
# is not the same as
super()
```
