# Ruby introspection cheatsheet

Given

```ruby
class Simple

  def initialize(x = 12, y = 3)
    @x = x
    @y = y
  end

  def do_stuff
    'stuff'
  end

  def other_stuff
    'stuff'
  end

end

sim1 = Simple.new
```

## names of instance variables

```ruby
ob.instance_variables
```

## values of instance variables

Two options

1. use the accessor readers (if they exist)
2. bash though with ???

## all methods available to the object

what about method_missing stuff?

```ruby
sim1.methods
```

## check whether an object responds to a method

```ruby
# does not check the ancestor chain
# but will return true for modeules mixed directly into Simple!
TODO: what exacty is going on here
sim1.respond_to? :some_method
sim1.respond_to?(:some_method, true) # also checks private methods

# checks all available methods (
sim1.methods.grep /some_method/

# can I tell waht an object will respond to with method_missing?
```

## all methods defined in the object's class (includancestors)in the object

TODO: check exactly what this does

```ruby
ob.methods - Object.methods
```

## see the unique object id

```ruby
sim1.object_id
```

## find which ancestor supplies a method

```ruby
sim1.method(:foo)
```

## find file and line num of a method

```ruby
sim1.method(:foo).source_location
```

##

```ruby

```
