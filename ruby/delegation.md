# Delegation in Ruby

Forwardable

# Delegator

```
require 'delegate' # provides SimpleDelegator and DelegateClass

class Foo
    def name

end

class FooDecorator < SimpleDelegator

    # any method in here can call super to invoke it's counterpart in the underlying object
end

decorated_foo = FooDecorator.new(Foo.new) # all methods sent to decorated_foo will be passed on to the instance of Foo we got in our constructor

underlying_foo = decorated_foo.__getobj__

another_foo = Foo.new
decorated_foo.__setobj__(another_foo) # now decorated_foo will pass on calls to another_foo


```

- use SimpleDelegator.new when the class you will delegate to must be decided at
  runtime (via the #**setobj** method)
    - SimpleDelegator is a subclass of Delegator
    - when you change the underlying object with #**setobj** it does **not**
      change the decorators methods - this means that you usually want to use
      the same type of underlying object
- use DelegateClass() when the class you will delegate to is fixed
