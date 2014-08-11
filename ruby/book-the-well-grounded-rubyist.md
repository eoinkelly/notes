# The well grounded rubyist

Think of ruby as 3 levels

1. core language
2. extenstions & libraries that _ship with_ ruby (and the facility for making
   your own.
3. command line tools that _ship with_ ruby.

todo: load my awesomeprint in irb and pry

Ruby code is made of _identifiers_

# Ruby identifiers

There are 4 types of identifier in ruby source code

Every element in ruby source code is one of these:

1. Variables (4 types)
  1. Local
  2. Instance
  3. Class
  4. Global
2. Constants
3. Keywords
4. Method names

1. Variables (4 types)
  1. Local
      * start with letter, _
      * contain: letters, _, digits
      * e.g. `some_var`
  2. Instance
      * start with @
      * contain: letters, _, digits (same as local vars)
      * e.g. `@some_var`
  3. Class
      * start with @@
      * contain: letters, _, digits (same as local vars)
      * e.g. `@@some_var`
  4. Global
      *  start with $
      * does not follow the same conventions as other types of variable
      * if it begins with a `$` its a global!
      * e.g. $LOAD_PATH, $:, $1, $/
2. Constants
    * begin with uppercase letter
    * e.g. `Foo`, `FOO_BAR`, `STDIN`
3. Keywords
    * e.g. `def`, `class`, `if`
    * approx 40 keywords in Ruby
4. Method names
    * same naming rules as local variable except they can _end in_ `!`, `?`, `=`
    * each of the extra naming suffixes has a meaning
        * `!` this method is dangerous
        * `?` this method returns true|false (is a predicate)
        * `=` this method is a setter for an instance variable
      QUESTION: does ruby enforce these rules?

Constructors in ruby can either be Foo.new(...) or some built-in objects have
special syntax

```ruby
"stringy" # String constructor
23        # Fixnum constructor
[3,5,6]   # Array constructor
{a: 4}    # Hash constructor
4.5       # Float constructor
```

There is a difference between sending a message and the method that implements
it. In ost case they are the same but with stuff like `method_missing` there is
no garuantee that the message name will match the method name


"some styntatic structures that help you create and manipulate objects are not
themselves objects"
TODO: what are these?


Bareword method calls (without an explicit receiver) use `self` as the receiver.
Within a class `self` is a reference to the class. At the global level `self` is
a refernce to the _default object_

```ruby
>> puts "hi"
# is really just ...
>> self.puts "hi"

>> self
main
>> self.class
Object < BasicObject
```

* Objects can get methods and behaviours that their class did not give them!
    * QUESTION: How?
    * Re-open the object and stuff to its eigenclass
* The class is responsibile for the object being born in memory (instantiation)
but once there, the object has a life of its own
* This is a defining principle of ruby code!

```ruby
class Foo
end

f = Foo.new

QUSTION: how do I add stuff to just object in ruby - see ruby under microsocope
```

