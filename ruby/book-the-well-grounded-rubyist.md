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

* Objects can get methods and behaviours that their class did not give them in
* ??? ways!
    1. Re-open the object and stuff to its eigenclass. There are two syntaxes
      for this:
          1. `def ob_name.method_name`
          2. `class << ob_name`
    2. Refinements
* The class is responsibile for the object being born in memory (instantiation)
  but once there, the object has a life of its own
* This is a defining principle of ruby code!

```ruby
class Foo
end

f1 = Foo.new
f2 = Foo.new

# Method 1: def ob.meth_name
############################

# add a method to f1 singleton class
def f1.thing
  puts "hi from thing"
end

# Method 2: <<
##############

class << f1
  # create new lexical scope, methods in here go in f1's singleton class
  def do_other_thing
    puts "other thing"
  end
end

# Method 3: Refinements
#######################

# Foo will have an extra method when this refinement is invoked
module SomeThing
  refine Foo do
    def local_thing
      puts "local thing"
    end
  end
end


# Usage
#######

f1.thing # => "hi from thing"
f2.thing # NoMethodError

f3 = Foo.new

f3.local_thing # NoMethodError

using SomeThing

f3.local_thing # => "local thing"
```

## # Checking syntax

Syntastic ruby checking uses `'-w -T1 -c` as args

* `-c` check syntax
* `-w` show warnings
* `-T1` run in $SAFE level 1 (turn on tainting checks)

# Taint & Trust

* Ruby considers user input tainted by default
* You can mark objects as `tainted` in ruby
* Use the _#taint_ and _#untaint_ methods to control taintedness
* _#tainted?_ checks the taintedness status of an object

```
1] pry(main)> str = gets
Hi there
"Hi there\n"
[2] pry(main)> str.tainted?
true
[3] pry(main)> str2 = "foo"
"foo"
[4] pry(main)> str2.tainted?
false
```

Ruby 1.9+ also has the notion of objects being trusted

_#trust_
_#untrust_
_#trusted?_

#### $SAFE

All information from the outside world can be marked as tainted. When running in
a safe mode, potentially dangerous methods will raise a SecurityError if passed
a tainted object.

* Ruby has differnt levels of paranoia you can access by setting `$SAFE`

more at http://phrogz.net/programmingruby/taint.html

* `RbConfig::CONFIG` is a big hash of the ruby interpreters config variables
* it seems to always be available in my irb/pry

* rubylibdir
    * contains the std lib (ruby files)
* archdir
    * runtime loadable C extensions (.so, .dll, .bundle)
* sitedir
    * a local parallel of the main ruby installation dir for your custom stuff
    * has its own archdir and libdir (`sitearchdir` and `sitelibdir`)
* vendordir
    * some third party stuff installs itself here
    * it is not yet clear whether it is best practice to install to here or
      sitedir


Q: do require and load force you to use files that end in .rb?

up to 1.2.4
