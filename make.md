# make

A makefile is

* a set of rules used to build an application
* the first rule seen by make is the _default_ rule
* A rule consists of three parts
    1. target
        * the **file** (or phony target) which must be made
    2. one or more prerequisites
        * **files** (or phony targets) which must exist before the commands can be run
    3. one or more commands
        * shell commands which will build the target from the prerequesites
* each command line
    * is passed to a separate shell
    * must begin with a TAB character
        * make will interpret any file which begins with tab as a command!!!
* make uses the existance of a file and its modification time to decide whether files need to be re-built
* the description file can be called
    * `Makefile`
    * `makefile`
    * `GNUmakefile` (note the lowercase 'm' in 'makefile' - it won't find your file if you name it `GNUMakefile`
* most targets are filenames but you can create "phony" targets which are not
    * you can think of a phony target as a shell script embedded in a make file
    * conventional phony targets
        * `all` - builds everything and is often the first (and therefore default) target in the file
        * `clean` - cleans up
    * you could create a filename with the same name as a phony target which
      would cause make to treat it as a file target with no prerequestites so
      it would never be run. Avoid this by making the phony target a
      prerequisite of the special, built-in, always out of date `.PHONY`
      target:
        ```makefile
        .PHONY: clean
        clean:
            rm -rf *.o
        ```
* you can have multiple targets in a rule to indicate that the commands in the rule will generate/update more than one file
* general shape of a rule
    ```makefile
    target1 target2 target3: prereq1 prereq2
        commands
        commands # each line runs in separate subshell
    ```
* make builds an internal dependency graph - a filename target can appear in
  more than one rule and make will just add it to the existing node in the
  graph
* rules run both "forwards" and "backwards".
    * Consider
        ```makefile
        foo.o: foo.c foo.h
            cc foo.c
        foo.h:
            cat first.h second.h > foo.h
        foo.c:
            cat banner.txt foo.inc.c > foo.c
        ```
    * In the "forward" direction if either `foo.c` or `foo.h` are changed (mtime changes) then `foo.o` must be rebuilt
    * the "forward" direction lets `make` decide what should be rebuilt based on file mtimes
    * In the "backward" direction if make wants to build `foo.o` for some reason then we have to ensure that `foo.c` and foo.h are up to date first.
    * The backward direction lets make know what has to be done to create a particular target
* make has 3 kinds of rules
    1. explicit rules
        * explicit filename targets and filename prerequisites
        * most rules you write will be explicit rules
    1. implicit rules
        * are built into make itself
        * are either:
            1. pattern rules
            2. suffix rules built-in to `make` itself
    1. pattern rules
        * work on wildcards instead of file names as targets
        * types
            1. regular pattern rules
            1. static pattern rules
* suffix rules are supported for compatibility with older make implementations but have been deprecated by pattern rules
* make has variables which have two syntaxes
    1. single character after $ - `$F`
    1. word in parentheses after $ - `$(FOO)`
* make has an `include` directive which will include makefile instructions from another file
* make supports bourne shell wildcards for both targets and prerequisites
    * note that wildcards in command lines are expanded by the shell, not make itself
    ```make
    prog: *.c
        $(CC) -o
    ```

## Invoking make

```
make # build default target
make targetname # build named target
make -n # dry run, print only what it would do to build target
```

### The -l special case for loading system libraries

https://www.gnu.org/software/make/manual/html_node/Libraries_002fSearch.html

> When a prerequisite’s name has the form ‘-lname’, make handles it
> specially by searching for the file libname.so, and, if it is not
> found, for the file libname.a in the current directory, in
> directories specified by matching vpath search paths and the VPATH
> search path, and then in the directories /lib, /usr/lib, and
> prefix/lib (normally /usr/local/lib, but MS-DOS/MS-Windows versions
> of make behave as if prefix is defined to be the root of the DJGPP
> installation tree).

> Although the default set of files to be searched for is libname.so and
> libname.a, this is customizable via the .LIBPATTERNS variable. Each word in
> the value of this variable is a pattern string. When a prerequisite like
> ‘-lname’ is seen, make will replace the percent in each pattern in the list
> with name and perform the above directory searches using each library file
> name.

> The default value for .LIBPATTERNS is ‘lib%.so lib%.a’, which provides the
> default behavior described above

QUESTION: what is vpath and VPATH?
QUESTION: how do I set .LIBPATTERNS var?
