## extern

-
- a way of creating global variables i.e. variables that are _external_ to all
  functions
- storage is allocated for the variable only once
- the variable is declared outside of any function (even main)
- functions that wnat to reference the global have to declare it (all variables
  in C must be declared before us) via the extern keyword
- you can omit the `extern` keyword iff
    - the external variable is declared higher up in the same source file
- if you want to reference globals from other files you _Must_ use `extern`
- global variables are usually declared in header files

int dostuff() { } // empty argument list provides backwards compatibility, turns
off argument checking, bad :-( int dostuff(void) { } // more explicit about "no
args", preferred option

Variable definition = the place where the variable is created a.k.a. storage is
allocated Variable declaration = the nature of the variable is declared but no
storage is allocated

```c
int max;

void do_things_to_max() {
    extern int max;
    ++max;
    print("%d", max);
}

do_things_to_max();
do_things_to_max();
do_things_to_max();
```

# Storage classes

- storage class determines
    1. the part of memory where memory is allocated for the variable.
    2. how long the allocation continues to exist

1. `auto`
    - there is an `auto` keyword but it is not commonly used
    - is the default storage class - if you omit a storage class when declaring
      a variable then `auto` is used.
    - `auto` variables are allocated when you enter a block and deallocated when
      you exit it.
    - access restricted to the block in which they are declared as well as any
      nested blocks
    - also know as local variables
2. `static`
    - like `auto` they are scoped to the function they are declared in
    - but they are only allocated once and retain their value after the function
      has exited.
    - good for any state that is
        - should not be available to any other function
        - needs to be tracked across invocations of that function
        - they are sort of a global that is restricted to just that function.

    ```c
    // x has not been allocated yet
    void foo()
    {
        // ??? It doesn't make sense to use a static with an initializer as it
        // will just get reset each time.
        static int x = 12;

        // how do you get an intiail value into a static var?
        static int counter;

        // for (...) { ...; counter++; }
    }
    // x is still allocated but not available here
    ```

    Q: what does static outside a function do?
    - `static` set on a global variable _or_ function restricts access to it to
      just the file it was defined in. It reads as "make this global but only to
      this file".
        - it is a primitive form or access control
    - functions are _external_ storage class by default but can be made static.

3. `register`
    - tells compiler you would prefer if this variable was stored in registers
      not in main memory
    - a way of suggesting to the compiler that we will need fast access to this
      variable - implementations do not garuantee they will do it.
    - they have the same scope as automatic variables (function local)
4. `extern`
    - declare without defining
    - The compiler converts a single `.c` file into a `.o` file. `extern` tells
      the compiler "hey don't worry about resolving this variable from the
      source file you are currently compiling - the linker will find the
      definition for you.
        - extern variables are compiled without the variable being resolved - it
          is resolved by the linking process.
    ```c
    extern int foo2; // alwasy a declaration (no memory allocated
    int foo3 = 12; // always a definition (memory is allocated)
    int foo1; // could either be definition or declaration depending on compiler
    ```

    - if used within a function it tells the compiler that you want to use the
      _global_ named variable
    - creates global variables
        - they last for the lifetime of the program
    - function declarations are implicitly extern
        - functions in C are external by default (available to all source files)
    - there is one box in memory and all source files can use it.
    - `extern int foo; this keyword tells the compiler that "hey this integer
      called foo already has storage allocated for it by some other file so
      don't make a new box".
    - to make a global
        - just declare the variable as normal in `file1.c` - it needs to be
          declared outside the scope of any function
        - reference it in any funcion in `file2.c` using `extern`
        - the variable name and type have to match obviously
    - the `extern` keyword is not used to make variables in the external area -
      you do that by delcaring them outside any functions.
        - external variables are created by the position of the declaration in
          the file.
        - all code following the declation can access that variable no matter
          what file it is in.
    - `extern` within a function:
        - `extern` is used to tell the compiler that you want to reference
          global variables from within your function.

# Memory organisation

- Text (or Code) area
    - where the app instructions are put
- Stack area
    - a LIFO queue
    - memory is alloacted and deallocated automatically by the running code
    - function parameters, automatic (local) variables are stored here
- Heap
    - `external` and `static` variables are allocated here
