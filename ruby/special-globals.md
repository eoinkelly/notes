
# Ruby's special globals

__FILE__
    * the path to the file that you are invoking it from
    * That path may be relative or absolute depending on your shell CWD relative to the file
        * this means that __FILE__ is cognisant of your shell CWD - its value
        * depends on it to an extent
    * it is created and stored _but never updated_ when your file is *loaded*
    * it is a path to this *file*, not the top-level file of your program
    * it is unique in every ruby file in your app e.g.
    * consequences:
        * `Dir.chdir` does nothing to this. it does not depend on the CWD of the process!
    * the value in __FILE__ depends on your shell CWD when you run the ruby process
        * it is just the filename if the file is in the same dir as your shell CWD
        * it is an absolute path if the file is in a different dir to your shell CWD

```ruby
# Load a file where I know its path relative to current file
# ##########################################################
require File.expand_path('../other.rb', __FILE__)
require File.expand_path('../other', __FILE__)

# Alternative (longer)
require File.expand_path('besideme.rb', File.dirname(__FILE__))

# Discussion
# * __FILE__ includes the current filename we use the `../` to get rid of it
# * The normal require rules about omitting the file extension apply
```




$:  $LOAD_PATH
    * Array of dirs that ruby will search for filenames passed to `load`
    * `.` is not on the load path. `load` is hardwired to search it first anyway

$"  $LOADED_FEATURES
$/
$stdin
