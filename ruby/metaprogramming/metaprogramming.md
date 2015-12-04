## module insertion

* `class A; include Foo; end` puts Foo above the current class/module in the ancestor chain
* `class A; prepend Foo; end` puts Foo below the current class/module in the ancestor chain
    * => Foo -> A -> Object -> BasicObject
## self

* `self` is like a global pointer
* "self" is a role in ruby
* there is exactly one object playing the "self" role at any time during
  execution of a ruby program
* method calls without an explicit receiver use self as the receiver
* when you call a method on an object ruby does
    1. set self to point to the other object
    2. walk other_object#class and then though the ancestors to find the method
    3. enter the method code, using `self` as the implicit reciver
* when you first enter a ruby program `self` points the `main` object
    * main is a object that ruby creates for you and it acts as the default context
    * this allows ruby to support "scripting" things like methods not in classes
* in a class or module definition (but outside of method definitions) `self` is set to the class or module itself


* class is a subclass of module
* Module is just a package/bundle of methods
* Class is a subclass of that
    * it combines a package of methods with a way to allocate memory for new
      objectsand a way for those objects to automatically get access to those
      methods
    * A Class is a Module with specialised "object allocation" behaviour

```ruby
class Foo
  self # Foo itself

  def do_thing
    self # the instance of Foo
  end
end

module Bar
  p self # => Bar

  def do_thing
    p self #
  end
end
```

If you prepend a module in a class it will be put before it in the ancestors chain
Class.superclass ignores modules so you won't see it in Thing.superclass

The (only???) way to see the method look up chain is to use .ancestors

```ruby
module A; def do_thing; puts "A"; end; end;
module B; def do_thing; puts "B"; end; end;
class Thing; include A; prepend B; def do_thing; puts "thing"; end; end

tt = Thing.new
tt.do_thing => "B"
Thing.ancestors
# [
#   [0] B,
#   [1] Thing < Object,
#   [2] A,
#   [3] Object < BasicObject,
#   [4] PP::ObjectMixin,
#   [5] Kernel,
#   [6] BasicObject
# ]
```


If something on the ancestors chain calls a method without an explicit receiver the ruby will always start the search with the object that `self` points at

=> if your module's methods call other methods in the same module those versions won't necessairly be used (it dependson the object that includes the module)
=> modules can only be understood in the context of the object that includes them - they may interact with each other
=> obj.class.ancestors is THE way to figure out where methods will come from
```
module Foo
    def do_thing
    do_other_thing
    # ruby will lookup this method starting at the self object so there is
    # no garuantee that the version of it defined in this object will be the
    # one called - it deends on the object that includes this module
    end

    def do_other_thing
    # ...
    end
end
```

If *anything* on the ancestors chain refers to an insance variable it is always an instance variable of whatever `self` points at
=> ancestors manipulate the state of the object
=> can think of the ancestors tree of being collapsed into a big list of methods that the object can access
=> no matter where the method comes from, the state it operates on is in the current object
