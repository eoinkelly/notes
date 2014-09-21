# The well grounded rubyist

Think of ruby as 3 levels of stuff

1. core language
2. extenstions & libraries that _ship with_ ruby (and the facility for making
   your own).
3. command line tools that _ship with_ ruby.


# Ruby identifiers

Thinking like a parser for a moment, *every* element in ruby source code is
oneof four types of _identifier_.

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

TODO: find out more about this - why doesn't rails use it?

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

## unix cwd

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

$: $LOAD_PATH
    * Array of dirs that ruby will search for filenames passed to `load`
    * `.` is not on the load path. `load` is hardwired to search it first anyway

## load

Load basically reads a file into a string and evals it. It is also smart enough
to look for the file in entries on the $LOAD_PATH and can wrap the contents of
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
            2. Each directory in global `$:` (`$LOAD_PATH`) array
    2. relative path
        * is assumed to be relative to the current working directory of the process (which can change)
        * it will **not** search $LOAD_PATH in this case
    3. absolute path
        * e.g. `/var/thing/foo.rb`
        * it will **not** search $LOAD_PATH in this case
* Notice
    * you don't have to manually add CWD to the $LOAD_PATH with `load`
    * the only time it will search $LOAD_PATH if you give it a _filename_!
    * _filename_ must have the `.rb` suffix - load does no magic here.
    * it looks for the file _directly_ in the dirs of $LOAD_PATH - it does not
      care about specific subdirectories the way ?? does.
* `load` is a method so will be executed at the point where ruby finds it in my file. Implications:
    * you can skip loads by putting them in conditionals
    * you can generate the filename argument dynamically
* load will not check to see if the file has been loaded already
    * sometimes this is very handy (e.g. playing with a file in _irb_) but most of the time it is not what you want.

### optional wrapping

```ruby
# loader.rb
load "loadee.rb"

# puts local_var        # hidden
# puts LocalConst       # hidden
# puts local_method     # leaks
# puts SomeClassName    # leaks because class names are attached to Object
# puts NASTY            # leaks because we explicitly attached the constant to Object
# puts $globally        # leaks because it is global

# load("loadee.rb", true)

# puts local_var        # hidden
# puts local_method     # hidden
# puts LocalConst       # hidden
# puts SomeClassName    # hidden
# puts NASTY            # leaks
# puts $globally        # leaks


# loadee.rb
local_var= 12
def local_method; 23; end
LocalConst = 34
class SomeClassName; end
::NASTY = 'hi'
$globally = "everywhere"
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

    TODO what is diff between these?

    > In no circumstance will any local variables in the loaded file be propagated to the loading environment.
    what exactly happens when you load a file that has unscoped variables

#### Aside: eval

what is binding?
you can pass binding as an optional second arg to pry
also binding.pry


basically wrapped load does an Module.new.instance_eval
    so self within it will point at the anonymous module which is thrown away
and normal load just does a file read and eval so it will share semantics with eval

=> to understand load, I need to understand eval
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

## require

Protects you from loading files more than once
    this can cause bugs depending on what the files do

require keeps a global list of all the files it has evaluated already
$LOADED_FEATURES
    an array of fully qualified filenames of every file that has been required
    using abolsute paths means that require won't be fooled by different strings
    require "./foo"
    require "foo"

require works with "features" as opposed to files to allow it to work with dynamic libraries - the idea si that the if I am using a gem I shouldn't know whether it is implemented as a ruby file or as a C dylib

require takes a "feature name" not a "file name"
it hten tries to resolve the "feature anem" to a "file"
this file could be ruby, C etc.

require returns true if it actually did the load, false if it was already loaded

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
      load the right one for your platform
Notice that `require` looks for the feature _directly_ in the dirs on $LOAD_PATH - it does not search sub directories or have any knowledge about the directory structure.

#### How does `require` not load the same code twice?

It uses `$LOADED_FEATURES` to keep track.

### require_relative

* its big feature is that it will start the search in the directory that the requiring file is in
* can be thought of as "require_relative_to_the_requiring_file"
* tries to load "foo" relative to the requiring file's path not just the `$LOAD_PATH`
* It removes the need to manually prepend stuff to the $LOAD_PATH

Like `require` it works on _features_ not _files_

### RubyGems

Before rubygems if you wanted to add a new feature to your system you had to

1. modify `$LOAD_PATH` so that the file could be found
2. Install it into a dir which is already on the load path e.g. `vendor_ruby` or `site_ruby`

This was bad because:

* File conflicts
* Painful uninstall
* No way to have multiple versions of libraries

Enter Rubygems

* All gems live within a gems directory
* Each gem version has its own container directory within the gems directory
* each version of each gem has its own code, docs, tests etc.

`Gem.path` is the path that ruby will use to find gems

* The `$LOAD_PATH` is used to find ruby files directly but the `Gem.path` just finds gem container directories. Once the gem container directory is found, the gemspec in there is consulted and the `require_path` paths are added to the $LOAD_PATH


`Gem.path` gets ruby to the gem container dir. Then rubygems reads the gemspecs in `specifications/`
`gemname.gemspec` file within it tells ruby what paths to add to the `$LOAD_PATH`

```sh
# To inspect your gem environment on command line:
$ gem env
$ gem env path
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

* rubygems overrides the built-in require with one that knows how to find features in gems
* The correct terminology is to "require a _feature_ **within** a gem"

#### GEM_HOME and GEM_PATH

GEM_PATH provides the locations (there may be several) where gems can be found.
GEM_HOME is where gems will be installed (by default).
(Therefore GEM_PATH should include GEM_HOME).

They don't seem to be set when using rbenv so they are obviously just one way to do it?

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

#### Other stuff

* You can disable ruby gems with `ruby --disable=gems`. This will revert `require` back to the built-in ruby version.

### auto loading

    TODO: implement a simple version of rails auto loading

### Command line tools

How do I check what command line tools are available in the current ruby?

```sh
$ ls -l `ruby -e "puts RbConfig::CONFIG['bindir']"`
```

* Note that ERB is built-in to ruby.

    QUESTION: can ERB be used for stuff other than HTML?

* Note that `ruby -v` shows version *and* enabled _verbose mode_. Use `ruby --version` to just see version string.

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

QUESTION: how does this match up with require?

# Extras

Modifying LOAD_PATH
Since we want our lib to be found first we prepend it to the array with `unshift` rather than appending it to the end.
