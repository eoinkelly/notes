# The well grounded rubyist

Think of ruby as 3 levels of stuff

1. core language
2. extenstions & libraries that _ship with_ ruby (and the facility for making
   your own).
3. command line tools that _ship with_ ruby.

# Ruby identifiers

Thinking like a parser for a moment, *every* element in ruby source code is
one of four types of _identifier_.

1. Variables (4 types)
    1. Local
    2. Instance
    3. Class
    4. Global
2. Constants
3. Keywords
4. Method names

In more detail:

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
        * start with `$`
        * can contain a bunch of special characters (unlike local, instance, class variables)
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
    * Ruby does enforce `!?=` being at the **end** of the method name e.g. `def foo?bar;
      puts "blah"; end` will create _#foo?_ that takes one arg

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

The book says:

>some syntatic structures that help you create and manipulate objects are not
>themselves objects

Some examples of these syntactic structures:

* blocks are not strictly object (but can be converted into objects)
* argument list - contains object but is not an object itself
* keywords e.g. if, else

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

# conditionals always evaluate to whatever object is returned by the last line of the
# section that executed. They evaluate to nil if no case matched

[1] pry(main)> if (true); "hi"; else; "bah"; end
"hi"
[2] pry(main)> if (false); "hi"; else; "bah"; end
"bah"
[3] pry(main)> if (false); "hi"; else; "bah"; 23; end
23
[4] pry(main)> if (false); "no"; end
nil

# same goes for case statements

[5] pry(main)> case "hi"; when "hi"; 23; 24; end
24
[6] pry(main)> case "hix"; when "hi"; 23; 24; end
nil
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
  but once there, the object has a life of its own. This is a defining principle of ruby code!

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

Rails apps work almost exclusively with tainted data so they don't use the
taint and trust mechanisms provided by ruby. Similarly running a rails app with
$SAFE set to a high level would probably stop it functioning.

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

## precursor: unix cwd

* Every unix process has a "current working directory"
* It is stored in the process by the Kernel
* If the process was spawned from a shell it will usually be set to whatever the
  CWD of the shell was.
* System calls like `chdir` and `getcwd` use this if you don't pass them an absolute path
* Shell built-ins like `pwd` and `cd` use it too

You can inspect the cwd of a process using

```shell
$ pgrep someprocessname
12345
lsof -p 12345| grep cwd
```

`Dir.pwd`
* prints the process' current working directory as a string

`Dir.chdir`
* Changes the current working directory **of the process** to the given string
* You can pass it a block which will change the dir for the block only (changes
  it back after the block ends - neat!

`$:` or `$LOAD_PATH`
    * Array of dirs that ruby will search for filenames passed to `load`
    * `.` is not on the load path. `load` is hardwired to search it first anyway

## load

Load basically reads a file into a string and evals it. It is also smart enough
to look for the file in entries on the `$LOAD_PATH` and can wrap the contents of
the string in an anonymous module for you to provide _some_ protection from it.

There is no "units of code" like other langs have - there is just text in files
that is evaluated.

```
load "foo.rb"

# roughly equivalent to ...

eval File.read("foo.rb")
```

* Signature: `load(FILENAME, wrap=false) # -> true`
* simpler than require - it _loads a file_ but require _requires a feature_
* `load` lives in `Kernel`
* _FILENAME_ is one of
    1. filename with extension: e.g. `foo.rb`
        * `load` will search for the file in
            1. The _current working dir_ of the process. Note this is probably
               not the directory that the including file is in!
            2. Each directory in global `$:` (`$LOAD_PATH`) array.
                * Note that the CWD of the process is *not* on `$LOAD_PATH` by
                  default
    2. relative path
        * is assumed to be relative to the current working directory of the
          process (which can change)
        * it will **not** search $LOAD_PATH in this case
    3. absolute path
        * e.g. `/var/thing/foo.rb`
        * it will **not** search $LOAD_PATH in this case
* Notice
    * you don't have to manually add CWD to the `$LOAD_PATH` with `load`
    * the only time it will search `$LOAD_PATH` if you give it a _filename_!
    * _filename_ must have the `.rb` suffix - load does no magic here.
    * it looks for the file _directly_ in the dirs of `$LOAD_PATH` - it does not
      care about specific subdirectories
* `load` is a method so will be executed at the point where ruby finds it in my
  file. Implications:
    * you can skip loads by putting them in conditionals
    * you can generate the filename argument dynamically
* load will not check to see if the file has been loaded already
    * sometimes this is very handy (e.g. playing with a file in _irb_) but most
      of the time it is not what you want.

### optional wrapping

```ruby
# ./loadee.rb
local_var= 12
def local_method; 23; end
LocalConst = 34
class SomeClassName; end
::NASTY = 'hi'
$globally = "everywhere"


# ./loader_default.rb
load "loadee.rb"

# puts local_var        # hidden
# puts LocalConst       # hidden
# puts local_method     # leaks
# puts SomeClassName    # leaks because class names are attached to Object
# puts NASTY            # leaks because we explicitly attached the constant to Object
# puts $globally        # leaks because it is global

# ./loader_wrapped.rb
load("loadee.rb", true)

puts local_var        # hidden
puts local_method     # hidden
puts LocalConst       # hidden
puts SomeClassName    # hidden
puts NASTY            # leaks
puts $globally        # leaks
```

If the optional wrap parameter is true, the loaded script will be executed
within an anonymous module, _somewhat_ protecting the calling program's global
namespace.

Hides
    * Constants
    * local variables
Does not hide
    * class names
    * method names
    * Constants created with `::SomeConstant`
    * `$global_variables`

There are a bunch of other ways that the loaded script can changed the global
environment so if you need real isolation you can

1. start a subprocess
2. use "fork and exec"

    TODO: what is diff between these?

Wrapped load does an `Module.new.instance_eval` so `self` within it will point
at the anonymous module which is thrown away and normal load just does a file
read and eval so it will share semantics with `eval`

#### Aside: eval

    TODO

#### Aside binding

Objects of class `Binding` encapsulate an _execution context_ at some place in
the code and store this context so you can use it again in future.

Things that are in an _execution context_:

* variables
* methods
* value of self
* an iterator block

* `Kernel#binding` will return a new binding that wraps up whatever execution context you are in when you called it.
* Instances of `Binding` can be passed as a second arg to `eval` to establish an environment for evaluation

Pry adds a `pry` method to all bindings and when you invoke `binding.pry` you are calling it on the current execution context


#### Aside:

When you create a constant in global scope in ruby it is added to `Object` - it
is **not** added to the main object (which is what `self` refers to in the
global scope in Ruby)

```
[21] pry(main)> Eoin = "hi"
"hi"
[22] pry(main)> self
main
[23] pry(main)> self.constants
NoMethodError: undefined method `constants' for main:Object
[25] pry(main)> Object.constants.grep(:Eoin)
[
  [0] :Eoin
]

```

## Aside: running command from specific gem

* rubygems will check the command you run for the first arg
* if first arg is wrapped in `_` e.g. `_1.2.3_` it will steal it and run the
  command from that specific version of the gem

## require

* Protects you from loading files more than once
    this can cause bugs depending on what the files do

* require keeps a global list of all the files it has evaluated already:
  $LOADED_FEATURES
    * an array of fully qualified filenames of every file that has been required
    * it uses absolute filename paths so that it won't be fooled by different
      strings
    ```
    # same file, different strings passed to require
    require "./foo"
    require "/Users/joe/code/foo"
    require "foo"
    ```

* require works with "features" as opposed to files to allow it to work with
  dynamic libraries - the idea si that the if I am using a gem I shouldn't know
  whether it is implemented as a ruby file or as a C dylib

* require takes a "feature name" not a "file name"
* it then tries to resolve the "feature name" to a "file" - this file could be
  ruby, C etc.
* require returns true if it actually did the load, false if it was already loaded

* `require(name) # -> true (or false if name is already loaded)`
    * can be called many times but will only load its argument the first time
    * it is more abstract than `load` - you _require_ a _feature_ not a _file_
    * if name does not "resolve to an absolute path" then $LOAD_PATH will be
      searched.
      ```
      require "foo" # will not load ./foo.rb
      require "./foo" # will load ./foo.rb
      ```
    * if `require` gets just a feature name it only searches the `$LOAD_PATH`
    * if `require` gets a relative or absolute path then it converts it to an
      absolute path and loads the feature from it
        * QUESTION: will it alos search load path if you give it a "pathish" string?
    * file name extensions
        * you can require
            1. ruby files `require "foo.rb"
            2. C code `require "foo.so"`
        * if name ends in rb it is loaded as a ruby file
        * if it ends in .so|.dll|.o it is loaded as a ruby extension
        * you can use any extension filename (.so|.dll|.bundle|.o) and ruby
          will load the right one for your platform
        * it name does not have an extension, ruby tries adding .rb|.so etc.
          until it finds it
    * require throws a `LoadError` if it fails to find the feature
    * The absolute path of the feature is added to `$LOADED_FEATURES` (alias
      `$"`) - this is how require keeps track of what is loaded
    * in a rails app there are 2000+ entries in $LOADED_FEATURES

Notice that `require` looks for the feature _directly_ in the dirs on
`$LOAD_PATH` - it does not search sub directories or have any knowledge about the
directory structure.

#### How does `require` not load the same code twice?

It uses `$LOADED_FEATURES` to keep track.

### require_relative

* its big feature is that it will start the search in the directory that the
  requiring file is in
* can be thought of as `require_relative_to_the_requiring_file`
* tries to load "foo" relative to the requiring file's path not just the
  `$LOAD_PATH`
* It removes the need to manually prepend stuff to the `$LOAD_PATH`
* Like `require` it works on _features_ not _files_
* A relative path passed to `require` will **always be relative to the CWD of
  the process** but a relative path passed to `require_relative` will be
  relative to the file you are currently in.

Aside: dunder = double underscore

```ruby
File.expand_path("../../some_file", __FILE__)
# or
require_relative '../some_file'
```

TODO: rebuild require function in ruby code

### RubyGems

Before rubygems if you wanted to add a new feature to your system you had to

1. modify `$LOAD_PATH` so that the file could be found
2. Install it into a dir which is already on the load path e.g. `vendor_ruby`
   or `site_ruby`

This was bad because:

* File conflicts
* Painful uninstall
* No way to have multiple versions of libraries

Enter Rubygems

* All gems live within a gems directory
* Each gem version has its own container directory within the gems directory
* each version of each gem has its own code, docs, tests etc.
* rubygems overrides the built-in require with one that knows how to find
  features in gems
* The correct terminology is to "require a _feature_ **within** a gem"

`Gem.path` is the path that ruby will use to find gems

```
[2] pry(main)> Gem.path
=> ["/Users/eoinkelly/.gem/ruby/2.2.0",
 "/Users/eoinkelly/.rbenv/versions/2.2.3/lib/ruby/gems/2.2.0"]
```

* The `$LOAD_PATH` is used to find _features_ - `Gem.path` finds gem container
  directories. `"#{Gem.path}/gem_name" is the gem directory
* Once the gem container directory is found, the gemspec in there is consulted
  and the `require_path` paths are added to the $LOAD_PATH

```ruby
# pseudocode for how gems are loaded

# read the spec for the required gem
spec = eval File.read("#{Gem.path}/specifications/#{some_gem_name}.gemspec")

# add all require_paths to $LOAD_PATH
spec.require_paths.each { |p| $LOAD_PATH << p }
```

```sh
# To inspect your gem environment on command line:
$ gem env
$ gem env path # same as `Gem.path` in irb
```

```ruby
p Gem::Specification.select(&:activated?).map { |gs| gs.full_name }
old_lp = $LOAD_PATH.dup
old_lf = $LOADED_FEATURES.dup

# When we require a feature while the gem aware require is loaded ...
#
require 'rake/file_list'

# ... that require will search through all of the gems it knows about looking
# for a file corresponding to the feature you have asked for.

# QUESTION: does it search in the existing load path before looking for the feature in gems?

# ... If there are multiple versions of a gem it gives preference to more
# recent versions ...

# ... When it finds the file corresponding to the feature ...

# ... it 'activates' the gem ...
puts 'Changes to activated gems:'
p Gem::Specification.select(&:activated?).map { |gs| gs.full_name }

# ... which adds its lib directory is added to $LOAD_PATH ...
puts 'Changes to $LOAD_PATH:'
p $LOAD_PATH - old_lp

# ... Then require proceeds as it usually would. If any `require` statements
# are found within the feature then they same process is followed for them.
puts 'Changes to $LOADED_FEATURES:'
lfs = $LOADED_FEATURES - old_lf
lfs.each { |lf| puts lf }
```

#### GEM_HOME and GEM_PATH

* GEM_PATH
    * the directories (there may be several) where gems can be found.
    * `gem env path`
* GEM_HOME
    * where gems will be installed (by default).
    * `gem env home`

Therefore GEM_PATH should include GEM_HOME

* Rubygems has built in environment which contains default values for these
* These are in rubygems env not the shell env by default i.e. use `gem env
  path` to see `GEM_PATH` not `env|grep GEM_PATH`

#### Why does "requiring a gem" seem to work?

* In reality `require` works with _features_.
* Gems can contain multiple _features_ (which can be implemented as either
  ruby code or compiled dynamic library)
* The _gem aware_ version of `require` that RubyGems provides
    1. Finds the gem that provides the feature
    2. Activates it which adds its `require_paths` to `$LOAD_PATH`
    3. Proceeds as the normal require would.

* There is a convention where you have a feature (implemented as a ruby file)
  in the `lib/` path of your gem. This is the feature that is loaded when you
  "require a gem". By convention this feature will load any other features that
  the gem needs.

Gems can have other features that you can load explicitly e.g. `require
"rake/file_list"` requires the `rake/file_list` feature which happens to come
from the `rake` gem.

You cannot "require a whole gem" but you can require a feature that loads all
the code in the gem.

#### The gem method

* `gem` is used as a workaround for require not understanding gems
* I think you would only need it on old versions of ruby whose `require` is not
  gem aware.

```ruby
# capture original load path
old_load_path = $LOAD_PATH.dup

gem 'bundler', '1.10.6'

# notice that the bundler gem /lib has been added to LOAD_PATH
p $LOAD_PATH - old_load_path
# => ["/Users/eoinkelly/.rbenv/versions/2.2.3/lib/ruby/gems/2.2.0/gems/bundler-1.10.6/lib"]

# require now works as usual
require 'bundler'
```
#### Reverting to ruby built-in require

* You can disable ruby gems with `ruby --disable=gems`. This will revert
  `require` back to the built-in ruby version.


#### Direct manipulation of $LOAD_PATH

You don't need to directly manipulate the `$LOAD_PATH` because `require` and
`require_relative` provide a lot of flexibility. It is still possible to do it
but is now an anti-pattern.

### auto loading

    TODO: implement a simple version of rails auto loading

### Command line tools

How do I check what command line tools are available in the current ruby?

```sh
$ ls -l `ruby -e "puts RbConfig::CONFIG['bindir']"`
```

* Note that ERB is built-in to ruby.
    * ERB can be used for files other than HTML!
* Note that `ruby -v` shows version *and* enabled _verbose mode_. Use `ruby
  --version` to just see version string.
* `ruby -e` can span many lines (until it is terminated by another `"`)
    * in zsh at least you cannot edit a line after you have hit <RET>
    ```
    ➜  ruby -e "
    dquote> puts 'this is my program'
    dquote> puts Time.now
    dquote> "
    this is my program
    2014-08-15 07:31:08 +1200
    ```
* `ruby -rfeaturename`
    * you can require features at the command line
    * `ruby -rzlib my_zlib_needing_script.rb`
    * `ruby -rtime -e "puts Time.now.iso8601"`
    * not sure what use case of this with a scirpt would be?
    * this is handy for one liners as it is more concise than `require
      "some_feature"`


You can see that most of them are small wrappers (except for ruby itself)

In ruby the value of an assigment expression is the RHS of the assigmnet

### irb

```
# options
--no-echo # don't show return value of expressions
```

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

UP TO END CHAPTER 1
# Chapter 2

* Objects in a program should not be constrained by the "real world"
* You can make an object to represent *anything* - the world of objects is an imaginary one!

* Classes are _one_ way in ruby to bundle and label behaviours
* Each object can learn behaviours that its class did not teach it
* The concept of "classes" is built on top of "objects" - not the other way round.


ruby can return multiple values - they get automatically wrapped by an Array

```ruby
[1] pry(main)> def foo; return 33, "hello", 4.3; end
=> :foo
[2] pry(main)> foo
=> [33, "hello", 4.3]
```

Ruby methods _always_ return some value

```ruby
[3] pry(main)> def bar; end
=> :bar
[4] pry(main)> bar
=> nil
```

Objects are "born" with innate behaviours  (from their class and modules) but can "learn" new behaviours at any time in their life


## BasicObject

* has 7 methods
    * ???
    * does not implement #methods so cannot inspect them

Public instance methods

1. #!
2. #!=
3. #==
4. #__id__ aka #object_id
5. #__send__ aka #send
6. #equal?
7. #instance_eval
8. #instance_exec

Private instance methods

1. #method_missing
2. #singleton_method_added
3. #singleton_method_removed
4. #singleton_method_undefined

* ruby defines `__send__` and `__id__` so that you can add your own `#send` and `#id` methods to objects and still get at the built-in ruby methods

## `*` in ruby

### Sponge parameters

```ruby
# usage 1: as a "sponge parameter"
# capture 0+ method args into a single array
# any args captured by *rest are optional
def do_thing(a, b, *rest)
end

# capture all args into an unnamed array (capture and ignore)
def do_other(*)
end

# sponge parameters have the lowest priority when ruby is matching arguments to
# formal parameters so the following works
def do_other(head, second, *rest, last)
end

```
* sponge parameters have lower priority than default parameters so ruby will
  try to override defaults before giving any args to the sponge
* ruby will bind _every_ named parameter to a local variable - even if it is
  bound to an empty array or nil. "If it's named, it's local"

* only one sponge param is allowed in each method signature

Each ruby method argument has to be in _one_ of these categories:

1. required - `a`
2. default valued - `a=:foo`
3. optional - `*a`

There are 2 ways to create local variables in ruby

1. explicit assignment
2. binding of method arguments to method parameters


All variables in ruby are _references_ to object except for the following _immediate values_

1. integers
1. symbols
1. true
1. false
1. nil

```
[12] pry(main)> false.object_id
=> 0
[13] pry(main)> true.object_id
=> 20
[14] pry(main)> nil.object_id
=> 8
[15] pry(main)> 1.object_id
=> 3
[16] pry(main)> 2.object_id
=> 5
[17] pry(main)> :foo.object_id
=> 1651548
```

In the case of the immediate values the variable holds the value directly not a
reference to it. In practice you don't really notice because all the immedaite
values above are immutable.

The immediate values are all basically immutable global variables

Ruby does not have pre and post increment operators because integers are stored
as immediate values. `11` is just a value and it does not know how to deal with
operators

There is only one `23` object in the system - all variables that contain `23`

```
x = 23
x++ #
x-- # syntax error
--x # noop
++x # noop

# ++ will add numbers
x ++ x # => 46
x + +x # => 46


23 -- 4 # => 27
is same as
23 - -4
23 ++ 4 # => 27
23 + +4
23 + 4  # => 27
23 - 4  # => 19
```

lvalues = local, class, instance variables
lvalues = things on the left side (or target) of an assignment
all ruby variables are really references to objects so if you pass them into a function they will mutate that variable

# clone and dup

* `#dup`
    * makes a "shallow" copy of an object - the ivars are copied but not the
      objects those ivars reference
    * modules that the object has been extended with will not be copied
    * copies tainted state
    * does not copy frozen state (DIFFERENCE)
    * does not copy the singleton class (DIFFERENCE)
    * dup typically uses the class to create a new instance
* `#clone`
    * makes a shallow copy of the object
    * copies frozen and tainted state of an object (DIFFERENCE)
    * copies the singleton class (DIFFERENCE)
    * duplicates an object including its internal state

Classes can implement `#initialize_copy` to tweak how clone and dup work


Important difference in ActiveRecord objects
* clone = create a new record with the same id so that when #save is called it
  will overwrite the existing record in the DB
* dup = create a new object with no id set so that #save will create a new
  record in the DB

* rails offers #deep_dup on Array, Hash, Object

# freeze

* freeze makes object immutable
* if you freeze and array it does NOT freeze each object in the array
