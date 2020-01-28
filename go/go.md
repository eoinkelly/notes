# Go Programming Language Book

# Questions/To figure out

```bash
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
* functions can return multiple values (separated by comma) e.g. `return a, b, c`
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
    * the idea is that "one thread per connection" in servers does not scale
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
    * are idealized - they don't have a type until you try to store them in some memeory cell
    * they are compiled away - they only exist at compile time - they are **not** read-only variables
    * this means that number constants can have arbitrary precision
    * introduced with `const`
    * must be initialized iwth a value (you cannot create a constant without an initial value)
    * they have a parallel type system
    * constants have the same variable naming rules as variables
        * the standard lib uses camel case (whether the first letter is upper/lower depends whether you want to export it from the package or not)
        * go variables and constants do not use snake_case - they use camelCase or CamelCase
        * there are a few POSIX constants which are all UPPERCASE because history e.g. O_CREAT
        * => go code does not try to visually differentiate constant names from variable names
    * can be typed or untyped - untyped constants are considered to have a "kind" not a "type"
        ```go
        123345 // literal constant of kind: integer

        const Aa = 123 // kind: integer
        const Ab = 123.345 // kind: floating-point

        // constants can store up to 256 bits - much more most of the actual types we will convert them to
        const Xx = 12319230384039398938749020948903034004982382334344248338377394383

        // this constnat is "locked down" to being an int - go lang can not change its type
        const Bb int = 123 //type: int

        // iota - https://github.com/golang/go/wiki/Iota
        // `iota` is a special value which starts at 0 and can be used when declaring a
        // block of related constants. It will automatically increment and be assigned
        // to the constants that follow even if they aren't explicitly assigned to it.
        //
        // It is useful when you want to make an enum alike thing
        const (
            Aa = iota // 0
            Bb        // 1
            Cc        // 2
        )
        ```
    * kinds are _implicitly_ converted by the compiler (this is unlike types which must be explicitly converted)
        * the language spec has rules about "kind promotion" so it can
* culture
    * use short name for vars that don't last long
    * use anonymous structs (type declared at same time as value initialized) - adding names adds "pollution"
* escape analysis
    * if your function returns a pointer to a value it created then go will figure that out ahead of time and put the value in the heap not the stack - this ensures that the value will be accessible to the calling code
    * we get better perf when values stay on the stack
        * but how much does it matter?
    * you only need garbage collection when the value is put on the heap - stack values are cleaned up automatially
* go stacks
    * go routine stacks start at 2k (compare to OS threads which start at 1M)
    * go stacks are allocated contigiously
      * "contigious memory is at the heart of mechanical sympathy"
    * at compile time we know the size of every stack frame
    * if a stack exceeds it's 2k allocation then **the whole stack** has to be moved because of the contigious allocation of stacks
    * when you stack size reduces back down, GC may move it back
    * because whole stacks can move, no go routine can have a pointer to any **other** goroutines _stack_ - pointers between frames on a single stack are fine
        * => if you want to share a value between goroutines it must move to the heap - go takes care of this for you
* the go heap
    * the GC manages the heap
* GC
    * uses a "pacing algorithm" to decide at what % of used heap to run GC
    * up to go 1.8 the GC had two phases of _stop the world_ - now it is still _stop the world_ but the latency is reduced
        * 1.8 was a significant release for GC - it greatly reduced GC latencies
    * GC is implemented as a group of go routines
    * if a goroutine starts allocating a lot of memory the GC will move it on to the same CPU as the GC is scheduled on

## scope

https://medium.com/golangspec/scopes-in-go-a6042bb4298c

Things that care about scope

1. variable declaration
2. constant declaration
3. type declaration

They all follow the same scoping rules.

Go has two main scopes

1. package global scope
   * constants and variables declared outside of a function are visable across the whole **package**
   * short variable declaration `:=` cannot be used
2. block scope
   * constants and variables declared within a block of `{}`
       * a function is the most common kind of block
   * short variable declaration `:=` can be used
3. file scope
   * **only** used for imports - file scope **does not exist** for variables and constants



## Syntax

### Built-in Types


* int
    * go will choose size of `int` based on the word size of the architecture you **compile** on. It ensures that the size of `int` matches your pointer size - this helps achieve some "mechanical sympathy" at the cost of having to be careful about overflows (again, I presume???)
* string
    * a 2 word data structure (i.e. size can be diff on diff architectures, 16 bytes on a a 64bit arch)
        * word 1: pointer to a backing array of bytes
        * word 2: length
* bool
* channel
* float
* array
* struct
    * composite type
    * contigiously allocated memory for the types which make up the struct
    * padding bytes are automatically added to line up nicely to alignment boundaries
    * rules are:
        * 1 byte values can be put wherever they land
        * _every 2 byte value must fall on a 2 byte boundary_
        * _every 4 byte value must fall on a 4 byte boundary_
        * _every 8 byte value must fall on a 8 byte boundary_
    * tip: order values within a struct in descending byte size to minimize padding overhead
    * Go "named types"  are nominally typed not structurally typed - if you have two structs with exactly the same shape but different names you cannot assign them to each other. You can explicitly do a conversion on them
    * if you careate an anonymous type then the compiler will compare shape and allow assignment
* ??? others

Go will always initialize values to whatever their "zero value" is (string = empty string, bool = false, int = 0, float = 0.0 etc.)

### increment and decrement

```go
i++
j--
```

* go has postfix incrementing only. There are no prefix versions - nice!
* IMPORTANT: these are statements not expressions (they do not return a value so cannot be used in a larger expression) - nice!

### for loops

```go
// general form
// for <initialization>; <condition>; <post> {
// }

for some_test() { // empty initialization and post aka a while loop
}

for { // infinite loop
}
```

* go for loops do the work of while loops too
* never put parens around the conditions
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
* returns the `index, value` from arrays and slices
* returns `key, value` from maps
* returns `index, unicode_code_point` (aka `index, rune`) on strings
* designed to be used with a for loop

```go
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
* using `var x TYPE_NAME` is probably better for variables you want to start at their "zero value"

### casting vs conversation

* go does not have _casting_ (where we tell the compiler to treat a value as a differnet type)
* go has _conversion_ - it will convert the old type to the new (and allocate new memory for the new value as required)
    * => conversion can lead to allocation

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

Aside: single quotes are used to delimit runes (see below)

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
## Misc

* `println`
    * prints to stderr not stdout
    * is "not guaranteed to stay in the language"
    * is similar but not the same as `fmt.Println` which writes to stdout
    * does not return a value (`fmt.Println` is different here and does return a value)

* Returning from functions
    * you can return multiple values
    * do all funcs return a value?
    * go functions declare their return value in their signature
    * go functions do not have to return a value (empty return type in signature)


* structs
     are passed around by value (unless you explicitly use a pointer)

```go
package main

type Thing struct {
	Name string
	Age  int
}

func main() {
	var eoin = Thing{Name: "Eoin Kelly", Age: 38}
	tryToMutateThing(eoin)
	displayThing(eoin)

	actuallyMutateThing(&eoin)
	displayThing(eoin)
}

func displayThing(t Thing) {
	println(t.Name)
}

// this does not work because structs are passed by value
func tryToMutateThing(t Thing) {
	t.Name = "Mutante!!!"
}

func actuallyMutateThing(t *Thing) {
	t.Name = "Mutante!!!"
}
```
