# The well grounded rubyist

Think of ruby as 3 levels of stuff

1. core language
2. extenstions & libraries that _ship with_ ruby (and the facility for making
   your own).
3. command line tools that _ship with_ ruby.


# Ruby identifiers

Thinking like a parser for a moment, *every* element in ruby source code is one
of four types of _identifier_.

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
      * otherwise same rules as local vars
      * e.g. `@some_var`
  3. Class
      * start with @@
      * otherwise same rules as local vars
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
    * each of the extra naming suffixes has a meaning (by convention, not
      enforced by ruby)
        * `!` this method is dangerous
        * `?` this method returns true|false (is a predicate)
        * `=` this method is a setter for an instance variable
    * Ruby does enforce any of `!?=` ending the method name e.g.
      `def foo?bar; puts "blah"; end` will create _#foo?_ that takes one arg

Constructors in ruby can either be Foo.new(...) or some built-in objects have
special syntax


```ruby
# Object constructors with special syntax:

"stringy" # String constructor
23        # Fixnum constructor
[3,5,6]   # Array constructor
{a: 4}    # Hash constructor
4.5       # Float constructor
:foo      # Symbol constructor

# others ???
```

There is a difference between sending a message and the method that implements
it. In most case they are the same but with stuff like `method_missing` there is
no garuantee that the message name will match the method name.


>some styntatic structures that help you create and manipulate objects are not
>themselves objects

* blocks are not strictly object (but can be converted into objects)
* argument list - contains object but is not an object itself
* keywords e.g. if, else
* others ???

**Everything in ruby does _evaluate_ to a single object!**

This is true not only of variables, object literals, and method calls, but of
control-flow structures, keyword-based statements, class and method definitions,
and everything else.

```ruby
# Empty class delcaration evaluates to nil
class Foo; end          # => nil

# but actually class declarations evaluate to whatever their last expression returns
class Bar; 4 + 5; end   # => 9


# ... so we can use this to get hold of the singleton class for the class
class Bar2; self; end   # => Bar2

# of course using #singleton_class is easier
Bar.singleton_class

# conditionals always evaluate to an object
# they evaluate to nil if no case matched
```

Bareword method calls (without an explicit receiver) use `self` as the receiver.
Within a class `self` is a reference to the class. At the global level `self` is
a refernce to the _default object_.

```ruby
>> puts "hi"
# is really just ...
>> self.puts "hi"

>> self
main
>> self.class
Object < BasicObject
```

* Objects can get methods and behaviours that their class did not give them in two ways
    1. Re-open the object and add stuff to its singleton class (eigenclass).
       There are two syntaxes for this:
          1. `def ob_name.method_name`
          2. `class << ob_name`
    2. Refinements
        QUESTION: do refinements add stuff to the singleton class too?
* The class is responsibile for the object being born in memory (instantiation)
  but once there, the object has a life of its own.
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

TODO: find out more about this

#### $SAFE

All information from the outside world can be marked as tainted. When running in
a safe mode, potentially dangerous methods will raise a SecurityError if passed
a tainted object.

* Ruby has differnt levels of paranoia you can access by setting `$SAFE`

more at http://phrogz.net/programmingruby/taint.html

TODO: find out why $SAFE isn't used much in rails?

### ruby config

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


Where does ruby code I use live?

1. within _ruby_ binary.
2. as an _extension_ loaded at runtime.

Where do extensions live?

1. In the standard library that ships with ruby
2. In bundles of code that I install (gems)
3. In my app

What language are extensions written in?

Extensions can be written in ruby or compiled native code (which can be any
language). _ruby_ (the binary) talks to compiled code through its _ffi_

TODO: find out more of details of ruby ffi? is it C only?

Terminology

* _feature_ is most general term (covers extensions and libraries)
* _extension_ implies it is written in C
* _library_ is a common term for ruby code loaded at runtime

How can I load code at runtime in ruby?

2. `load`
1. `require`
1. `require_relative`
3. `gem` ???

# load vs require
* `load(FILENAME, wrap=false) # -> true`
    * simpler than require
    * _filename_ can be
        * filename: `foo.rb` will search file in
            1. _current working dir_ CWD - this will change if you change CWD at
               runtime
            2. each dir in global `$:` (`$LOAD_PATH`) array
        * absolute path
            * load will just load the file without using `$:`
        * relative path
            * is assumed to be relative to the CWD (so this will change if CWD
              does
    * _filename_ must have the `.rb` suffix - load does not fill it in for you!
    * is a method so will be executed at the point where ruby finds it in my
      file. Implications:
        * you can skip loads by putting them in conditionals
        * you can generate the filename arg dynamically
    * load will not check to see if the file has been loaded already - sometimes
      this is very handy (e.g. playing with a file in _irb_) but most of the
      time it is not what you want.
    * if _filename_ does not resolve to an absolute path then `$:` is searched
    * local variables from the loading file will never be available in the
      current file
    * _wrap_ decides whether ruby should execute the loading code within an
      anonymous module to protect the calling environments globals
    * lives in `Kernel`
    * In no circumstance will any local variables in the loaded file be
      propagated to the loading environment.

    * If the optional wrap parameter is true, the loaded script will be executed
      under an anonymous module, protecting the calling program’s global
      namespace.
        * but I can still make globals in the loaded file that will be available
        * in the main file so how is the namespace "protected" ???

* `require(name) # -> true (or false if name is already loaded)`
    * can be called many times but will only load its argument the first time
    * it is more abstract than `load` - you _require_ a _feature_ not a _file_
    * if name does not resolve to an absolute path then $LOAD_PATH will be
      searched.
        including . ???
    * you can require
        1. ruby files `require "foo.rb"
        2. C code `require "foo.so"`
    * if name ends in rb it is loaded as a ruby file
    * if it ends in .so|.dll|.o it is loaded as a ruby extension
    * it name does not have an extension, ruby tries adding .rb|.so etc. until
      it finds it
    * It throws a `LoadError` if it fails to find it
    * The absolute path of the feature is added to `$LOADED_FEATURES` (alias
    * `$"`) - this is how require keeps track of what is loaded

    * in a rails app there are 2000+ entries in $LOADED_FEATURES
    * you can use any extension filename (.so|.dll|.bundle|.o) and ruby will
      load the right one for yoru platform

* `require_relative`
    * `require_relative "foo"`
    * tries to load "foo" relative to the requiring file's path
    * searches relative to the dir of the script doing the loading!!
        * it does not search relative to the overall programs CWD!!
    * its big feature is that it will start the search in the dir that the requiring file is
      in so it avoids the need to manually add CWD to $LOAD_PATH

* `gem`
    TODO: learn about this

TODOL CWD in ruby - what is it by default, how to change it

TODO: implement a simple version of rails auto loading

$: $LOAD_PATH
    * Array of dirs that ruby will search for filenames passed to `load`
    * `.` is not on the load path. `load` is hardwired to search it first anyway


### Command line tools

How do I check what command line tools are available in the current ruby?

```sh
$ ls -l `ruby -e "puts RbConfig::CONFIG['bindir']"`
```

* Note that ERB is built-in to ruby.
Q: can ERB be used for stuff other than HTML?

* Note that `ruby -v` shows version *and* executes the provided script in
* verbose mode.

* `ruby -e` can span many lines (until it is terminated by another `"`)
    * in zsh at least you cannot edit a line after you have hit <RET>
* `ruby -rfeaturename`
    * you can require features at the command line
    * `ruby -rzlib my_zlib_needing_script.rb`
    * not sure what use case of this with a scirpt would be? handy for one
      liners.

```
➜  Desktop  ruby -e "
dquote> puts 'this is my program'
dquote> puts Time.now
dquote> "
this is my program
2014-08-15 07:31:08 +1200
```

You can see that most of them are small wrappers (except for ruby itself)

In ruby the value of an assigment expression is the RHS of the assigmnet

irb
--no-echo # don't show return value of expressions

### Rdoc

RDoc format

TODO: get the basics down of this
http://ruby-doc.org/gems/docs/r/rdoc-4.1.1/RDoc/Markup.html

Tools

* `ri`
    * lets you view extraced docs
* `rdoc`
    * extracts doc comments from source files

```sh
$ ri String#upcase
$ ri String::some_class_method

# no pager
$ ri -T String#upcase

```

### Rake

* `rake`
    * -T shows only tasks that have descriptions
    * -P shows all tasks and their prerequisites


### gem

~/.gem stores ???

GEM_HOME
GEM_PATH

gem install foo
gem uninstall foo


```ruby
# when you want to use a gem but not the most recent version installed on your
# system:
gem "foo", "3.6.9"
```

Q: how does this match up with require?
