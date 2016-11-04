# Go Programming Language Book

# Questions/To figure out


```
# WARNING: this deletes the 'go' binary too!!!
go clean -i all
# the go team are sitting on a change which would create a 'gopath' target that
# does what I wanted here - see https://go-review.googlesource.com/#/c/9780/3

# apparently this is the way to clean binaries out of your GOPATH
# TODO: what is the magic ...
go clean -i  ./...
```

## Overview

* `go env` shows you relevant env variables (including `GOPATH`)
* go has no repl (there is a website that fakes one)
* `go get` uses GOPATH as the base location for the files it downloads and builds

* a package maps to a single directory
    * each .go file in the dir declares the package it is part of at the top
    * QUESTION: do package declaration and dir name have to match?
* program will not compile if you have unused imports
* no semicolons required unless you put multiple statements on same line
* if a variable is not explicitly initialized it is automatically initialized to the "zero" for its type
    * this makes it safe to use without explicit initialization
    * examples
        * 0 for integer
        * "" for string
* go is garbarge collected
* entry point is function `main` in package `main` (by convention this is `main.go` file)
    ```go
    package main
    func main() {}
    ```
* functions can return multiple values
    * ignore a value by assigning to `_`
    * note that `_` is not a variable name - it actually tells the go compiler
      not to assign the variable
* passes args to functions as copies (just like C) so use pointers if
    1. you want to mutate the args
    1. you are passing big args
* arrays are fixed size and homogenous
* go does not let you overload functions but a few built-in functions are overloaded e.g. `make`
* arrays are _values_ in go, not pointers to anything
    * => when you pass them to a function a copy is made. TODO: check this
* interfaces
    * interfaces are declared explicitly but satisfied implicitly
        * a form of duck typing that is checkable at compile time
    * interfaces are usually small (0-2 methods)
    * an object can satisfy many interfaces
    * the "empty interface" is an interface with no methods that all objects
      satisfy
        * this is useful for creating methods can can take any object - you can
          say the satisfy the empty interface
* goroutines
    * like a thread but lighter weight
    * it is normal to have 10k goroutines in progress on a single machine
    * stacks are small, "segmented", sized on demand
        * segmented??
    * goroutines are muxed by demand onto real threads
    * the ideas is that "one thread per connection" in servers does not scale
      well as the kernel can only support so many threads
    * goroutines use channels to communicate
    * channel
        * a typed, synchronous, communication mechanism
        * a built-in type in the language
        * they are first-class in the language
* pointers
    * go uses ordinary C pointers
    * moves pointers from stack to heap as required to prevent dangling pointers
    * indexing is bounds checked
    * no pointer arithmetic allowed
        * => go is memory safer but not 100% safe
* public and private
    * things that begin with Uppercase are public from the package
    * things that begin with lowercase are private to the package
* go has no implicit conversions between types
* constants
    * are idealized - they don't have a type until you try to store them in
      some memeory cell
    * this means that number constants can have arbitrary precision

## Syntax

### Built-in Types

* int
* string
* bool
* channel
* float
* array
* ??? others

### increment and decrement

```go
i++
j--
```

* go has postfix incrementing
* IMPORTANT: these are statements not expressions (they do not return a value so cannot be used in a larger expression) - nice!
* they are postfix only. There are no prefix versions - nice!

### for loops

```
// general form
// for <initialization>; <condition>; <post> {
// }

for some_test() { // empty initialization and post aka a while loop
}

for { // infinite loop
}
```

* go for loops do the work of while loops too
* never parens around the conditions
* always have braces
* there are three main parts
    * initialization
    * condition
        * executed at the start of each loop
        * loop finishes unless it evaluates true
    * post
* better than C
    * initialization is optional
    * post is optional
    * if post and initialization are empty then separating `;` not required

### range

* iterates over data structures
* works on array, slice, map, string etc.
* returns the `index, value` form arrays and slices
* returns `key, value` from maps
* returns `index, unicode_code_point` (aka `index, rune`) on strings
* designed to be used with a for loop

```
// for loops can be used with the `range` keyword
// this whole range call and assignment is in the "condition" position of the for loop
for index, value := range someArrayOrSlice {
}

// use _ if you want to ignore one of the values range returns
for _, value := range someArrayOrSlice {
}


```

### :=

* part of _short variable declaration_
* can be used to declare one or more variables and automatically give them types based on the initial values
    * QUESTION: how does it work for more than one var?
    * seems to be a sort of type inference


## Tools

### Built-in tools

* go fmt
    * automatic code formatting
* go run
    * compiles to temp dir and runs code
        ```
        $ go run main.go
        # will build the file into some temp dir e.g.
        #     /var/folders/ct/g9q0hlv5481_t996htnj8pth0000gn/T/go-build788399466/command-line-arguments/_obj/exe/main
        # and then run it.
        ```
    * `go run --work main.go` to see where tmp dir is
* go get
    * downloads _and_ installs package
    * will also check for updates with `-u` flag
    * accepts all the flags that `go build` does to control builds
    * looks for a branch or tag to match the current version of go
* go clean
    * cleans out build artefacts
    * most of go compiler temporary build outputs are already in a tmp dir - `go clean` removes things to do with C builds
* go install
    * installs a go "package"
    * `go install <path/to/project/containing/a/main/dot/go>`
    * given a path inside your GOPATH will build it and put it in
      $GOPATH/bin/project_name
    * When you "install" a go package you
        1. pull down the src git repo
        2. build the git repo
        3. install that final binary into $GOPATH/bin

### Extra tools

* gocode
* goimports
    * given a file name it will find all the required imports and add them to the imports statement
* godef
* oracle
* golint
* gorename
* errcheck
* gotags
* golint
* go deps
* go vet
    * run some heuristics on code that look for errors that the compiler might
      not find

### Package management tools

Many package management tools available:

* glide
    * ++ aims to provide an actual package manager
    * provides a yaml config file and lock file
    * ++ compatible with built-in go tooling
    * https://github.com/Masterminds/glide
* govendor
    * https://github.com/kardianos/govendor
* godep
    * https://github.com/tools/godep
* gb
    * https://getgb.io/

#### the ... pattern

An import path is a pattern if it includes one or more "..." wildcards,
each of which can match any string, including the empty string and
strings containing slashes.  Such a pattern expands to all package
directories found in the GOPATH trees with names matching the
patterns.  As a special case, x/... matches x as well as x's subdirectories.
For example, net/... expands to net and packages in its subdirectories.

### string literals

Go has two forms of string literal

1. raw string literal
    * delimited by back quotes
    * not interpoloated
    * may contain newlines
    * backlash has not special meaning
    * can be multi-line as newline has no special meaning
2. interpolated string literal
    * delimted by double quotes
    * special chars denoted by `\` are interpolated
    * cannot be multiline

```go
foo := `this
is a multiline
raw string literal`

boo := "this is an interpolated string \nand must be on a single line"
```

### rune (character) literals

A rune literal represents a rune constant, an integer value identifying a Unicode code point

* delimited by single quotes

## Variable declaration

```go
// var <NAME> <TYPE>
// * notice no extra stynax just `var` keyword, variable name, variable type
//   separated by whitespace
var x int // int
var y [3]int // array of three int
var r io.Reader
```

Go is lexically scoped using blocks”. Basically this means that the variable exists within the nearest curly braces { } (a block) including any nested curly braces (blocks), but not outside of them.
