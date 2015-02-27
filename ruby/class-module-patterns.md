## Pattern: `extend self`

```ruby
module Capybara
  module Helpers

    extend self

    # def self.do_stuff
    def do_stuff
      # ...
    end
end
```

* any code placed within a class or module (not in a method within it) will
    be executed _when the class object is evaluated by ruby_
* It has the same result as if we put `self.` in front of _every_ method in the class
* Used as a shortcut when you know that _all_ methods in this module should be class methods
* Sometimes thought of as an anti-pattern (rightly so IMHO)

### Alternatives

```
module Utility
  class << self
    def utility_function1
    end
    def utility_function2
    end
  end
end
# -- same problem as `extend self` in that you could easily miss that all these
#    are class methods

module Utility
    def self.utility_function1
    end
    def self.utility_function2
    end
  end
end
# -- it does not as quickly say that *all* methods in here will be class methods
```




include
    * add methods to an instance of a class

extend
    * add methods to the a class object itself


The same module can be added either to the class itself or to instances of the class

