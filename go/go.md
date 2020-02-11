# Go

## Sources

* Go programming language book
* Go in action
* O'Reilly video series about go

## Questions/To figure out

```bash
# WARNING: this deletes the 'go' binary too!!!
go clean -i all
# the go team are sitting on a change which would create a 'gopath' target that
# does what I wanted here - see https://go-review.googlesource.com/#/c/9780/3

# apparently this is the way to clean binaries out of your GOPATH
# TODO: what is the magic ...
go clean -i  ./...
```

## CPU architecture

golang makes a big deal of not abstracting away the physical machine from you and how that lets you get good performance

Consider Intel Core i5 (same as my laptop)

> 14 nm, 8th Generation "Coffee Lake" 2.3 GHz Intel "Core i5" processor
> (8259U), with four independent processor "cores" on a single silicon
> chip, 128 MB of eDRAM embedded on the processor die, and a 6 MB shared
> level 3 cache.

* 4 cores
* each core has
    * private L1 instruction cache (32kb) and data cache (32kb) (total L1 = 64kb)
    * private L2 (256kb) cache
* all cores share one L3 cache (4MB)
* my cpu has 64 MB of eDRAM (embedded memory) on the processor die - this functions as a sort of "L4 cache"
* main memory (16GB)

Latencies for this chip

    Retrieve value from L1          ~ 4     clock cycles (+/- 1)
    Retrieve value from L2          ~ 12    clock cycles
    Retrieve value from L3          ~ 40    clock cycles (varies by part a bit)
    Retrieve value from eDRAM       ~ ???   clock cycles
    Retrieve value from Main mem    ~ 107   clock cycles

When CPU pulls value from main mem it pulls in increments of 1 "cache line" (64bytes)

Prefetchers

* prefetchers are the bit of the CPU that tries to put memory as close as possible ot theCPU when it needs it
* prefetchers depend on _predictaable access patterns to data_ - if we write code like this we are kind to the prefetcher
    * => from the hardware perspective the array is the most important data structure

Aside: practically speaking anything in L1 and L2 is also in L3

Table lookaside buffer (TLB)

* maintains OS page offsets to where the physical pages are
* misses on the TLB are often worse than cache misses
    * miss = need a value from a page whose address map isn't cached in the TLB

> If performance matters then the total amount of memory you have is the
> total amount of cache. main mem is so slow that practically speaking it might
> as well not even be there.
> - Scott Meyers (verbal quote so prob not 100% accurate)

In summary, use slices when you can because they are friendly to the prefetcher in the CPU

## Overview

* the omission of garbage collection from the _alef_ language in Plan9 made "concurrency too painful" - I presume this is why go has a GC
* go has no default parameter values
* go structs and arrays hold their elements "directly" which requires less storage and fewer allocations and pointer indirections than languages which use indirect fields
    * I presume this means that a go struct stores all its data in a contigious block and only has pointers if you explicitly have them in your code
* `go env` shows you relevant env variables (including `GOPATH`)
* go has no repl (there is a website that fakes one)
* `go get` uses GOPATH as the base location for the files it downloads and builds
* a go package is one or more .go files with the same `package nameOfPackage` declaration
* go standard library is of the "batteries included" world view
* interop with C
    * go can call C without too much fuss - see the `C` pseudo-package
    * calling go from C is bit painful IMHO
* package `main` is a special package name which go considers to define a standalone executable
    * within package `main` the function `main` is considered to be the entry point of that executable
    * **All** `init` functions in any required package are run before `main` is
    * the `main` function
        1. has an empty parameter list
        2. has an empty return list
    * entry point is function `main` in package `main` (by convention this is `main.go` file)
        ```go
        package main
        func main() {}
        ```
* `os` package provides functions and values for dealing with the operating system in a platform independent way
    * `os.Args` is a string slice which holds the program command line args starting with the program name
    * error handling is "go like" i.e. failing calls return error values not error codes
* `syscall` package contains any OS specific stuff
* go does not permit unused local variables
* `:=` short variable definition
    * can only be used within functions - you can not use it at package level
* `bufio` package
    * helps with IO
* built-in functions
    * `make(type, size)`
        * allocates memory for a
            1. slice
            2. map
            3. channel
        * takes a type as first argument
        * takes some integer args which mean different things to each type
        * returns the actual value allocated, not a pointer to it (`new` returns a pointer)
    * `new(???)`
        * TOD
* a package maps to a single directory
    * each .go file in the dir declares the package it is part of at the top
    * QUESTION: do package declaration and dir name have to match?
* imports
    * program will not compile if you have unused imports
* no semicolons required unless you put multiple statements on same line
* if a variable is not explicitly initialized it is automatically initialized to the "zero" for its type
    * this makes it safe to use without explicit initialization
    * examples
        * 0 for integer
        * "" for string
* go is garbarge collected
* arrays are fixed size and homogenous
* go does not let you overload functions but a few built-in functions are overloaded e.g. `make`
* arrays are _values_ in go, not pointers to anything
    * => when you pass them to a function a copy is made. TODO: check this
* public and private
    * things that begin with Uppercase are public from the package
    * things that begin with lowercase are private to the package
* go has no implicit conversions between types
* the go spec no longer uses the term "reference type" because it is a poorly defined term  -see https://github.com/go101/go101/wiki/About-the-terminology-%22reference-type%22-in-Go


### culture

* use short name for vars that don't last long
* use anonymous structs (type declared at same time as value initialized) - adding names adds "pollution"

### escape analysis

* if your function returns a pointer to a value it created then go will figure that out ahead of time and put the value in the heap not the stack - this ensures that the value will be accessible to the calling code
* we get better perf when values stay on the stack
    * but how much does it matter?
* you only need garbage collection when the value is put on the heap - stack values are cleaned up automatially

### go stacks

* go routine stacks start at 2k (compare to OS threads which start at 1M)
* go stacks are allocated contigiously
    * "contigious memory is at the heart of mechanical sympathy"
* at compile time we know the size of every stack frame
* if a stack exceeds it's 2k allocation then **the whole stack** has to be moved because of the contigious allocation of stacks
* when you stack size reduces back down, GC may move it back
* because whole stacks can move, no go routine can have a pointer to any **other** goroutines _stack_ - pointers between frames on a single stack are fine
    * => if you want to share a value between goroutines it must move to the heap - go takes care of this for you

### go heap

* the GC manages the heap

## GC

* uses a "pacing algorithm" to decide at what % of used heap to run GC
* up to go 1.8 the GC had two phases of _stop the world_ - now it is still _stop the world_ but the latency is reduced
    * 1.8 was a significant release for GC - it greatly reduced GC latencies
* GC is implemented as a group of go routines
* if a goroutine starts allocating a lot of memory the GC will move it on to the same CPU as the GC is scheduled on

## Unsafe

* go has an `unsafe` package to allow you to step around the type system if you really need to

## Reflection

* go apparently has good reflection support via `reflect` package
## Scope

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
       * a function is the most common kind of block but go is not "function scoped
   * short variable declaration `:=` can be used
3. file scope
   * **only** used for imports - file scope **does not exist** for variables and constants

Go is lexically scoped using blocks. Basically this means that the variable
exists within the nearest curly braces { } (a block) including any nested curly
braces (blocks), but not outside of them.

You can use curly-braces to introduce a new variable scope within a function:

```go
package main

func main() {
	var x = "hello"

	// You can use curly-braces to introduce a new variable scope within a function
	{
		var y = "there"
		println("y:", y)
	}

	// println("y:", y) // compile error
	println("x:", x)
}
```

* if an entity is declared within a block it is scoped to that block.
* if an entity is declared outside of any block then it is visible to all files in the same package
* Note that you never scope an entity to a file in go - it is always scoped to a package or a block

## Built-in Types

### int

* go will choose size of `int` based on the word size of the architecture you **compile** on.
* It ensures that the size of `int` matches your pointer size - this helps achieve some "mechanical sympathy" at the cost of having to be careful about overflows (again, I presume???)

Go's integer types are:

    uint8, uint16, uint32, uint64, int8, int16, int32 and int64

* 8, 16, 32 and 64 tell us how many bits each of the types use.
* uint means “unsigned integer” while int means “signed integer”.
* Unsigned integers only contain positive numbers (or zero).
* In addition there two alias types:
    1. `byte` which is the same as uint8 and
    1. `rune` which is the same as int32.
* There are also 3 machine dependent integer types:
  1. uint
  2. int
  3. uintptr
* They are machine dependent because their size depends on the type of architecture you are using

### string

* literals declared with double quotes or backticks (single quotes create runes)
    * double quotes create a "normal" string
    * backticks allow you to use multiline strings
* are UTF-8 encoded by default
* are in effect a _read only slice of bytes_
    * => you cannot use a for loop and index offsets to edit a string
* a 2 word data structure (i.e. size can be diff on diff architectures, 16 bytes on a a 64bit arch)
    * word 1: pointer to a backing array of bytes
    * word 2: length
* the "zero value of a string is (nil, 0) (in pseudocode)
* For dynamic strings the backing array is put on the heap
* For static strings the backing array will be in the data segment
* strings are immutable, once created their contents cannot be changed
* `len()` will give you length of string in **bytes** (note not codepoints!)
* loop behaviour
    * a raw `for` loop on a string will yield the bytes in the slice
    * a range for loop will yield the runes of the string
* we don't call a string a "reference type" because a string zero value is considered "empty" but a reference zero value is considered "nil"
* In Go strings are not sequences of runes, they are utf-8 encoded sequences of runes.

### rune

* literal runes are declared with single quotes
* is an alias for an int32
* used by go to repsrent a unicode codepoint as an int32 - it does **not** store it UTF-8 encoded
* strings are `[]uint8` but you can iterate across them by rune (`range` does this out of the box)
* In Go strings are not sequences of runes, they are utf-8 encoded sequences of runes.

```go
s := "some string" // slice of uint8 aka bytes
runes := []rune(s) // slice of int32 aka rune
```

### bool

### channel

### floats (float32, float64 complex64, complex128)

Go has two floating point types:

1. float32 (single precision)
2. float64 (double precision)

as well as two additional types for representing complex numbers (numbers with imaginary parts):

1. complex64
2. complex128

Generally we should stick with float64 when working with floating point numbers.

### array

* a contigious block of memory
* all values must be same type
* the type of an array is made up of it's size and the element type i.e. an array of N things is not the same type as an array of N+1 things
* all arrays have a known size at compile time
  * => you cannot use a variable to declare the size of an array at compile time
* arrays are mutable
* can be sparse
* arrays are the key underlying data store in go
* **are passed by value between functions (i.e. a full copy is made)**
    * this can be very expensive so be careful
    * in practice slices are used to avoid this

```go
// initialize and array with values
a1 := [3]string{"a", "b", "c"}

// automatically set length based on num values provided
a1 := [...]string{"a", "b", "c"}

// initialize a sparse array
a1 := [10]string{2: "a", 4: "b", 6: "c"}

// init an array of pointers to ints
a2 := [3]*int{new(3), new(5), new(6)}
```


### struct

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
* if you create an anonymous type then the compiler will compare shape and allow assignment
* are passed around by value (unless you explicitly use a pointer)

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

#### struct tags

* Declaration of struct fields can be enriched by string literal placed afterwards — tag. Tags add meta information used either by current package or external ones.
* IMPORTANT: the tags are attached to the **type** not to instances of the type!
* Either raw string literals or interpreted string literals can be used but conventional format described below **requires raw string literals** because it uses `"` to delimit values.
* tags are available through the `reflect` package which allows runtime reflectiono
* tags strings can have any format but there is a "conventional format"
    * Tags are formatted as a concatenation of `key:"value"` pairs.
    * Pairs can be optionally separated by spaces — `key1:"value1" key2:"value2" key3:"value3"`.
* If conventional format is used then we can use two methods of struct tag (StructTag) — `Get` or `Lookup`. They allow to return value associated with desired key inside tag.
    * Return value of Get or Lookup is unspecified if tag doesn’t have conventional formato
* A key is often the name of the package e.g. `json`.
* Tag information is not checked when deciding whether types are assignable

```go
package main

import (
	"fmt"
	"reflect"
)

type car struct {
    // struct with tags in "conventional" format
	name      string `src:"blahsrc" reliability:"22" blank:""`
	numWheels int    `src:"vehicle"`
}

func main() {
	ferarri := car{"Ferarri", 4}

	fmt.Println(ferarri)

	t := reflect.TypeOf(car{})            // get the type
	nameField, _ := t.FieldByName("name") // get a field within the type

	// access the tag on the field
	fmt.Println(nameField.Tag)

	// Lookup returns the value and a boolean indicating whether there was a value
	val, exists := nameField.Tag.Lookup("src")
	fmt.Println(val, exists)

	// Get is like lookup but discards the boolean
	fmt.Println(nameField.Tag.Get("src"))
}
```

### slices

* a 3 word data structure
    1. a pointer to the backing array
    2. length
    3. capacity
* a **dynamically sized** sequence of array elements
    * presumably stored as an offset and a length like rust does ???
* slices have syntax sugar which make working with them feel like an arary
* `s[i]` to get an individual element of a slice
* `s[m:n]` to get a contigious subsequence of a slice
* `len(s)` to get length of slice
* created by the built-in `make` function
* the "empty struct" is a zero sized type used internally by go
    * see the example below for how `var` and `:=` differ in what they create


* Built-in `append(slice, value)` function appends values to the slice
    * note that append receives a **copy** of the slice header and returns a mutated version of the same slice header **value** it received
    * => you have to assign the return value of append to something!

```ruby
# pseudocode for implementation of append
func append(slice, value) do
    if len(slice) == cap(slice) then
        if cap(slice) <= 1024 # TODO: not sure if it exactly 2014
            create a new backing array of length 2 x cap(slice)
        else
            create a new backing array of length 1.25 x cap(slice)
        end

        copy the values from the old backing array to new
        edit the slice header pointer to point at the new backing array
        edit the slice header len to be the new len
        edit the slice header cap to be the new cap
    else
        add the given value to the backing array of the slice we received
        edit the slice header len to be the new len
    end

    return the slice header value (note not a pointer)
end

```

```go
type struct user {
    name string
    age int
}

var ex_1 []user
ex_2 := []user{}

// ex_1 creates a slice with pointer=nil, len=0, cap=0
// ex_2 creates a slice with pointer=emptystruct, len=0, cap=0
```

### intervals

* go intervals are "half open" i.g. `m:n` includes value at `m` but not value at `n` (it stops at `n-1`
    * it lets you use `len(s)` as the marker for the end of a slice subsequence and have it not be off by 1
        * `s[0:len(s)]` gets whole slice
        * `s[:len(s)]` gets whole slice (first value in interval defaults to `0`)
        * `s[0:]` gets whole slice (second value in interval defaults to `len(s)`)
        * `s[3:len(s)]` gets everything except first two elements

### maps

* an unordered collection of key value pairs
* implemented using a hash table
* good for large collections of data that need randome access
* the order of iterating across a map is deliberately random so you don't start relying on a particular order
* under the hood each bucket is
    1. an array of single byte values, each one being the high order byte from the output of the hash function
    2. an array of bytes which stores the key-value pairs
* any value can be a map value
* the following values cannot be map keys
    1. slice
    1. function
    1. struct which contains a slice
* can iterate across them with `range`
* A map "value" is actually a pointer to the underlying map data structure so
  when you pass a map to a function, only the pointer bit is copied i.e. a map
  function arg is shared between caller and callee
* remove a key from map with built-in `delete()` function

```go
// create a map with make
m1 := make(map[string]string)

// create a map with map literal (more idiomatic)
m2 := map[string]string{ "a": "b", "c": "d"}
```

There are two ways to get value from a map:

```go
// get value and flag if missing
// this will work even if the zero value for the value type is valid i.e. this
// can tell the diff between an empty string value and a missing value
val, exists = m2["a"]
if !exists {
    log.Fatal("blah")
}

// get value yolo
val = m2["a"] // val will be the zero value for the value type if the key doesn't exist

```



### Functions

* functions can return multiple values (separated by comma) e.g. `return a, b, c`
    * returning more than 2 values is unusual
    * the most common idiom is to return the value and also an error value
    * ignore a value by assigning to `_`
    * note that `_` is not a variable name - it actually tells the go compiler
      not to assign the variable
* passes args to functions as copies (just like C) so use pointers if
    1. you want to mutate the args
    1. you are passing big args
*
When you create a value in a function that you want to share with your caller (i.e. back up the stack) it is strongly recommended to create the value (not ap ointer to it) and then share the address of it in the `return` statement - this makes it more obvious that what you are returning is a pointer (not copying a pointer value)
## Syntax

### init()

* each package (including `main`) can declare 0 or more init functions - they are all run (in the order defined) before `main()` runs
    * I have read some stuff that `init` funcs in packages is bad pattern because it can have surprising side-effects. That makes sense but I don't know how closely that advice is followed in practice.

```go
func init() {
    // do init stuff in here
}
```

### built-in functions

#### make()

* creates channels, slices and maps

#### new

### string literals

Go has two forms of string literal

1. raw string literal
    * delimited by back quotes
    * not interpoloated
    * may contain newlines
    * backlash has not special meaning
    * can be multi-line as newline has no special meaning 2. interpolated string literal * delimted by double quotes * special chars denoted by `\` are interpolated * cannot be multiline

Aside: single quotes are used to delimit runes (see below)

```go
foo := `this
is a multiline
raw string literal`

boo := "this is an interpolated string \nand must be on a single line"
```

### rune (character) literals

* A rune literal represents a rune constant, an integer value identifying a Unicode code point
* delimited by single quotes

Q: is a rune a char literal? or different somehow?

### Variable declaration

```go
// var <NAME> <TYPE>
// * notice no extra stynax just `var` keyword, variable name, variable type
//   separated by whitespace
var x int // int
var y [3]int // array of three int
var r io.Reader
```
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
* always returns a **copy** of the values you are iterating over (the indexes are created just for the loop)
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

### Names

* go has 25 reserved keywords
* go has ~36 "predeclared names" for
    1. built-in constants e.g. `true`
    2. built-in types e.g. `int`
    * they are part of the pseudo-package `builtin` (the package exists so godoc has something to use)
        * `builtin` is not a thing you can actually import (it is an error to try)
    * predeclared names are NOT reserved because ???
        * book says there are a "handful of cases" where redeclaring one of these makes sense. example ???
    ```go
    package main

    func main() {
    	// this works. WTF.
    	var bool int = 45
    	println(bool)
    }
    ```
* the case of a name determines visibility across package boundaries
    * uppercase is "exported" visible outside of package
    * lowercase is visible only to code in same package
* package names are always lowercase
* go variables are camelcase by convention

### declarations

* There are 4 kinds of entity in a go program
    1. variables `var`
    2. constants `const`
    3. types `type`
    4. functions `func`
* each kind of entity has a "declaration" (introduced by a keyword) which specifies some **or** all of its properties

What makes up a go program?

* one or more `.go` files
* each `.go` files
    1. package declaration
    2. 0-many import declarations
    3. a series of package level declarations of variables, constants, types, functions **in any order**
        * => The order of declarations does not matter - you can refer to entities before their declaration in source code

Details of the declarations:

1. variables `var`
2. constants `const`
3. types `type`
4. functions
    * introduced by keyword `func`
    * contains
      1. name
      2. list of parameters
      3. optional list of results
          * is omitted if function does not return anything
      4. function body
          * delimited by `{}`
          * the opening `{` **must** be on the same line as the rest of the function declaration
          * contains all the statements that define what the function does
              * note they say 'statements' here not 'statements & expressions'

#### := short variable declaration

* it will error if no new variables are declared on the LHS
  * it's fine with re-declaring variables as long as there is at least one new variable

* go likes short variable names for variables which are declared and used close together

#### _ the blank identifier

* _ allows use to have values we don't care about
* tells the compiler we don't want to bind the value to a variable name - you can't use it as a variable name

### interfaces

* interfaces are declared explicitly but satisfied implicitly
    * a form of duck typing that is checkable at compile time
* interfaces are usually small (0-2 methods)
* an object can satisfy many interfaces
* the "empty interface" is an interface with no methods that all objects
    satisfy
    * this is useful for creating methods can can take any object - you can
        say the satisfy the empty interface

### goroutines

* like a thread but lighter weight
* it is normal to have 10k goroutines in progress on a single machine
* stacks are small, "segmented", sized on demand
    * segmented??
* goroutines are muxed by demand onto real threads
* the idea is that "one thread per connection" in servers does not scale
    well as the kernel can only support so many threads
* goroutines use channels to communicate

### channel

* a typed, synchronous, communication mechanism
* a built-in type in the language
* they are first-class in the language

### pointers

* go uses ordinary C pointers
* moves pointers from stack to heap as required to prevent dangling pointers
* indexing is bounds checked
* no pointer arithmetic allowed
    * => go is memory safer but not 100% safe

### constants

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

### errors

The built-in defn of `error` is

```go
type error interface {
	Error() string
}
```

so any type which has an `Error()` method can be used as an error.

It is usual for packages to extend it:

```go
package network

type Error interface {
    error
    Timeout() bool   // Is the error a timeout?
    Temporary() bool // Is the error temporary?
}
```

* `nil` value represents "no error"
* `err := errors.New("blah")`
    * returns an error that formats as the given text.
    * **Each call to New returns a distinct error value even if the text is identical**
* errors can be nested
    * an error wraps another error if it has an `Unwrap()` method which returns the other error - Simple!
* compare errors via `errors.Is(errA, targetErr)`
    * this will search nested errors on `errA` to see if any of those match `targetErr`

```go
err := fmt.Errorf("user %q (id %d) not found", name, id)
```

* it seems like errors are intended to be created as package level values
    * that way you can compare the error you got to the value it might be

### defer

https://blog.golang.org/defer-panic-and-recover

 * A defer statement defers the execution of a function until the surrounding function returns.
* The deferred call's arguments are evaluated immediately, but the function call is not executed until the surrounding function returns
* Deferred function calls are pushed onto a stack. When a function returns, its deferred calls are executed in last-in-first-out order.
* you can defer a call to a func
* deferred code runs **even if the function panics**
* Deferred functions may read and assign to the returning function's named return values.
In many ways defer'd functions act a bit like "finally" clauses in other langs e.g. `ensure` in Ruby.
You can use defer to change/replace the return value in the case of an panic

You can use the `recover` function insided a defered function to recover from a panic

When a function panics

1. it stops executing
2. it run any defered functions it has (in LIFO order)
3. it returns to the caller but behaves like a call to panic in the caller so go back to step 1.
4. When it gets to the top of top of the stack the program crashes

Recover from panic

* `recover()` function does nothing under normal execution
* if `recover()` is run during a panic then it captures the value given to panic and resumes normal operation

* you can use `panic()` and `recover()` to simulate an exception flow ???
    * is that accurate? what at the differences?

Go recommends that you keep panics within package boundaries - use defer functions with recover() to convert them into errors

> If you're already worrying about discriminating different kinds of panics, you've lost sight of the ball.
> Rob Pike

```go
package main
func main() {
    doThing() // => 2
}

func doThing() int {
    i := 1

    // inlined defer functions allow interesting stuff
    defer func() {
        // change the return value of doThing()
        i++
    }
}
```

```go
package main

import (
	"fmt"
	"reflect"
)

func main() {
	fmt.Println("Hello, playground")
	fmt.Println("return value of c():", c())
}

func c() (i int) {
	defer func() {
		err := recover()
		if err != nil {
			// there is something to recover from
			fmt.Println("c() had error:", err, reflect.TypeOf(err))
			// runtime error: index out of range [2] with length 2
		} else {
			fmt.Println("c() went fine")
		}
	}()
	aa := []int{33, 44}

	fmt.Println("val from array", aa[2]) // try to get an index which doesn't exist
	return 1
}
```

## Tools

### Built-in tools

* go fmt
    * automatic code formatting
* go run
    * compiles to temp dir and runs code
        ```bash
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
    * run some heuristics on code that look for errors that the compiler might not find

Go has **many** linters - we use golangci-lint to run them all

## Package management

### Legacy

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

### Modern

TODO

## Miscellaneous

### the ... pattern

An import path is a pattern if it includes one or more "..." wildcards, each of which can match any string, including the empty string and strings containing slashes.  Such a pattern expands to all package directories found in the GOPATH trees with names matching the patterns.  As a special case, `x/...` matches x as well as x's subdirectories. For example, `net/...` expands to net and packages in its subdirectories.

### println

* `println`
    * prints to stderr not stdout
    * is "not guaranteed to stay in the language"
    * is similar but not the same as `fmt.Println` which writes to stdout
    * does not return a value (`fmt.Println` is different here and does return a value)

### returning from functions

* Returning from functions
    * you can return multiple values
    * do all funcs return a value?
    * go functions declare their return value in their signature
    * go functions do not have to return a value (empty return type in signature)

