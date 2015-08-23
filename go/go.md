# Go

Status: Working my way through little go book

Docs: https://golang.org/doc/

Other resources

http://www.golangbootcamp.com/book
https://tour.golang.org/welcome/1

### Built-in tools

* go fmt
    * automatic code formatting
* go run
    * compiles to temp dir and runs code
    * `go run --work main.go` to see where tmp dir is
* go get
    * downloads _and_ installs package
    * will also check for updates with `-u` flag
    * accepts all the flags that `go build` does to control builds
    * looks for a branch or tag to match the current version of go
* go clean
    * cleans out build artefacts
    * most of go tools use a tmp dir
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

## Go syntax

* go is garbarge collected
* entry point is function `main` in package `main`
    ```go
    package main
    func main() {}
    ```
* variables are automatically initialized to an appropriate "zero" value for
  that type when created.
* functions can return multiple values
    * ignore a value by assigning to `_`
    * note that `_` is not a variable name - it actually tells go not to assign
      the variable
* passes args to functions as copies so use pointers if
    1. you want to mutate the args
    1. you are passing big args
* arrays are fixed size and homogenous
* go does not let you overload functions but a few built-in functions are overloaded e.g. `make`
* arrays are _values_ in go, not pointers to anything
* interfaces
    * interfaces are declared explicitly but satisfied implicitly
        * a form of duck typing that is checkable at compile time
    * interfacesa are usually small (0-2 methods)
    * an object can satisfy many interafces
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
    * no pointer arithmetic
    * => go is memory safer but not 100% safe
* public and private
    * things that begin with Uppercase are public from the package
    * things that begin with lowercase are private to the package
* go has no implicit conversions
* constants
    * are idealized - they don't have a type until you try to store them in
      some memeory cell
    * this means that number constants can have arbitrary precision

### Built-in Types

* int
* string
* bool
* channel
* float
* array
* ???
