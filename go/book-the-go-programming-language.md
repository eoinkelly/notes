# The Go Programming language

* the omission of garbarge collection from the `alef` language in Plan9 made "concurrency too painful" - I presume this is why go has a GC
* go has no default parameter values
* go structs and arrays hold their elements "directly" which requires less storage and fewer allocations and pointer indirections than languages which use indirect fields
    * I presume this means that a go struct stores all its data in a contigious block and only has pointers if you explicitly have them in your code
* Chapter 1-5 are the basics
* go has an `unsafe` package to allow you to step around the type system if you really need to
* go apparently has good reflection support via `reflect` package

END INTRODUCTION

## 1. Tutorial

* a go package is 1 or more .go files
* go standard library is of the "batteries included" world view
    * Aside: it kind of has to be given that go interop with C is ropey
* package `main` is a special package name which go considers to define a standalone executable
    * within package `main` the function `main` is considered to be the entry point of that executable
    * the `main` function
        1. has an empty parameter list
        2. has an empty return list

* `os` package provides functions and values for dealing with the operating system in a platform independent way
    * error handling is "go like" i.e. failing calls return error values not error codes
    * `os.Args` holds the program command line args starting with the program name
* `syscall` package contains any OS specific stuff
* slices
    * a **dynamically sized** sequence of array elements
        * presumably stored as an offset and a length like rust does ???
    * `s[i]` to get an individual element of a slice
    * `s[m:n]` to get a contigious subsequence of a slice
    * `len(s)` to get length of slice
* intervals
    * go intervals are "half open" i.g. `m:n` includes value at `m` but not value at `n` (it stops at `n-1`
        * it lets you use `len(s)` as the marker for the end of a slice subsequence and have it not be off by 1
            * `s[0:len(s)]` gets whole slice
            * `s[:len(s)]` gets whole slice (first value in interval defaults to `0`)
            * `s[0:]` gets whole slice (second value in interval defaults to `len(s)`)
            * `s[3:len(s)]` gets whole slice (second value in interval defaults to `len(s)`)
* go does not permit unused local variables
* `:=` short variable definition
    * can only be used within functions - you can not use it at package level

END SECTION 1.2

## 2. Program Structure

### Scope

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

### Names

* go has 25 reserved keywords
* go has ~36 "predeclared names" for
    1. built-in constants e.g. `true`
    2. built-in types e.g. `int`
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

UP TO END SECTION 2.2
