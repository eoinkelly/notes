# Decorator pattern with delegation

The standard library provides two delegation methods

1. `class WrapperClass < SimpleDelegator`
2. `class WrapperClass < DelegateClass(OriginalClass)`

In either case you need to `require delegate`

## SimpleDelegator

* To use
    * have the wrapper class inherit from SimpleDelegator
    * have wrapper initializer take instance to be wrapped and pass it to
      SimpleDelegator via `super(instance)`
* allows you to call `super` (note: no parens) in methods in wrapper to call
  corresponding method in wrapped class
* makes `decorated_foo == foo` work
* takes care of many edge cases that you would have to deal with if you just used `method_missing` to make the wrapper
* automatically forwards method calls to the wrapped class
* -- the augmented class does not know about the methods in wrapped class until
  it is actually wrapping an object
    ```rb
    AugmentedFoo.instance_methods.include? :blah # nope
    ```
* ++ we don't have to say which class we are wrapping at design time
* ++ if you want to wrap a range of similar classes use SimpleDelegator
* -- slightly slower method invocations than DelegateClass

TODO: super vs super()
```rb
class Foo
  def woof
  end
  def blah
  end
end


class AugmentedFoo < SimpleDelegator
  def initialize(foo)
    super(foo) # calls the initialize for Foo
  end

  def woof
    puts "special"
    super # call foo#woof
  end
end

af = AugmentedFoo.new(Foo.new)
af.woof
af.blah

# Downside: the wrapper doesn't know about methods in the wrapped class until
it has # been given an instance of it - this makes some metaprogramming harder
AugmentedFoo.instance_methods.include? :blah # false
af.instance_methods.include? :blah # true

```

## DelegateClass

* Is acutally a global method not a class constant
* the method generates a new anonymous base class for us at runtime
* ++ slightly faster method invocation than SimpleDelegator as it doesn't have to check for the existance of the method in the wrapper
* ++ better introspection than SimpleDelegator - see instance_methods example above
* -- requires us to say exactly which class we are wrapping at design time - a bit harder to test

```rb
class AugmentedFoo2 < DelegateClass(Foo)
  def initialize(foo)
    super(foo) # calls the initialize for Foo
  end

  def woof
    puts 'special'
    super # call foo#woof
  end
end
```

