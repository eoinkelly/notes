# Ruby special variables

* Ruby special variables are _usually_ not global despite their `$` prefix
* They are often thread local or even method local

```ruby
global_variables # => returns array of symbols of the names of global variables
```

ruby -p -a -e 'someprogram'

* `$LOADED_FEATURES`
    * scope ???

* `$LOAD_PATH`
    * scope ???
    * mutable

Process related

* `$0` or `$PROGRAM_NAME`
    * scope: ???
    * mutable
    * name of the current program
    * memonic for `argv[0]` in C
    * ru
* `$*` or ``
    * mutable
    * scope: ???
    * program args
    * conceptually `argv[1..]` in a C program
    * it does not include program name like C does
* `$$` or `$PROCESS_ID` or `$PID`
    * scope: ???
    * instance of `Integer`
    * the current process ID
* `$?` or `$CHILD_STATUS`
    * scope: ???
    * exit code of the most recent subprocess
    * instance of `Process::Status` not `Integer` (unlike `$PROCESS_ID`)

IO related

* `$_` or ``
    * `gets` and `readline` implicitly store the last line the read in this variable
    * `print` called without arg will output the value of this variable
    * designed for use in one-line scripts on command line
* `$<` or `ARGF`
* `$>` or `$stdout`
* `$;` or `$RS` or `$INPUT_RECORD_SEPARATOR`
    * input record separator
    * designed for one-liner usage
* `$\` or `$ORS` or `$OUTPUT_RECORD_SEPARATOR`
    * output record separator
    * designed for one-liner usage

* `` or ``

Exception related

* `$@` or `$ERROR_POSITION`
    * scope: ???
    * returns Array of backtrace or current exception
    * equivalient to `$!.backtrace`
* `$!` or `$ERROR_INFO`
    * scope: ???
    * the currently active exception


Regexp related

* These variables are aliases for class level methods - both the variable
  **and** the class method are thread local. Class methods you write are not
  thread local.
* The `English` gem provides no aliases for these variables

```ruby
str = "2017: ERROR: some message"
re = /ERROR|WARN/
```

* `$~` or `Regexp.last_match`
    * scope: thread local

* `$&` or ``
    * scope: thread local
    * shows the full previous match

* `$`` or ``
    * scope: thread local
    * the part of the string before the match
* `$'` or ``
    * scope: thread local
    * the part of the string after the match


* `$1`, `$2` etc.
    * the value of that capture group within the previous match



