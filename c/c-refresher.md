# C

UP TO END CHAP 1 in K&R BOOK

* has no IO built-in - all IO is done by functions (not part of the language)
* has no operators to work with composite data structures
* The second edition of the _C Programming Language_ book (the K&R book)
    * came out in 1988 (first edition was 1978)
    * is a companion to ANSI C standard aka C89 (1989 was when ANSI standardised) aka C90 (1990 was when ISO standardised it)
    * made some minor changes to the language
        * documented the stdlib for the first time
* C99
    * introducted new features (many of which compilers had already introduced)
        * one-line comments
        * variadiac macros
        * `complex` type for complex numbers
        * `long long int`
    * macro `#define __STDC_VERSION__ = 199901L` to enable C99 features
* C11
    * adds numerous new features to C and the library, including
        * type generic macros,
        * anonymous structures,
        * improved Unicode support,
        * atomic operations,
        * multi-threading,
        * bounds-checked functions.
        * It also makes some portions of the existing C99 library optional, and improves compatibility with C++
    * macro `#define __STDC_VERSION__ = 201112L` to enable C11 features
* Embedded C
    * Historically, embedded C programming requires nonstandard extensions to
      the C language in order to support exotic features such as fixed-point
      arithmetic, multiple distinct memory banks, and basic I/O operations.
* compiler and C versions:
    * clang
        * defaults to `GNU C11 mode`  (an extended version of c11 mode)
        * `gcc -std=c11`
    * gcc:
        * `gcc -std=c99`
        * `gcc -std=c11`
        * gcc 5 defaults to `GNU C11 mode` too (same as clang)
* is not stongly typed but has some type checking
    * you are not allowed to treat pointers and intergers as the same thing
    * you can use an explicit conversion
* was an evolution of BCPL an B (both of which were typeless)
* Variables can be
    * internal to a function
    * visible to just the source file they are declared in
    * available globally

## functions

* can be available
    * globally
    * just in the current file
* all function arguments are passed by value in C - everything is copied!
    * you can "modify" an argument if it is a pointer and you mutate what it points at but the pointer is still copied
    * the name of an array in C is a pointer to its first value so the whole array is not copied, instead the pointer to the first value is
* `int` is the default return type of functions in C - if you do not specify a return type the `int` is assumed (but compiler will probably warn you)

consider

```c
int do_thing(int a, int b) { return a + b; }
int x = 3, y = 4;
do_thing(x, y);
```

when `do_thing` is called first _copies_ of `x` and `y` are created and assigned as local variables `a` and `b` in `do_thing`


## Data types

* C fundemental data types are characters, integers (many sizes), floats (many sizes)
* C has derived data types via pointers, arrays, structures, unions

type                            | format specifier  | num bytes on my mac
------------------------------- | ----------------- | -------------------
char (a single byte)            | %c                | ? bytes
int                             | %d                | ? bytes
unsigned int                    | %u                |
long int                        | %ld               |
long (alias for `long int`)     | _see above_       | _see above_
short int                       | ? | ?
short (alias for `short int`)   | _see above_       | _see above_
float                           | %f                | ?
double                          | %lf               | ?

* all variables must be declared before they are used
* a declaration is a type followed by 1+ variable names (separated by commas)

* in C the `char` type is basically the "single byte" type, it can hold any byte value, not just printable ASCII

> In any context where it is permissible to use the value of a variable of some
> type, you can use a more complicated expression that returns that type.

* character constants in C e.g. 'A' (note single quotes for character constant, double quotes for string constant) are just another way of writing a small integer


Assigment associates from right to left so

```c
a = b = c = 0;
// is equivalent to
(a = (b = (c = 0)));
```

* Expressions connected by `||` or `&&` are
    * evaluated left to right
    * garuanteed to stop as soon as the truth or falsehood is known
        * this is important for using it as a short-circuit operator later

if `c` is a character representing a digit you can get its numeric value by evaluating `c - '0'` because numeric characters are stored in order in ASCII at least

## Arrays

* name of array is a pointer to its first element
    * implications
        * when array is passed to function the pointer is copied but not the array contents
* array declaration includes 1)element type and 2)no. of elements to allow the compiler to reserve storage

## String literals

* are put in the code (`.text`) segment by the compiler so are implicitly read-only
* have type `char[]` or `wchar_t[]` (`#include <wchar.h>` to use the wide char type)
    * the no. of elements in the array is the no. of chars + 1 for the
      terminating null character
* string literals have the type `char[]` not `const char[]` but _morally_ they should!
    * BUT it is "undefined behaviour" (usually a segfault) to try and modify the contents
    * this seems to be a historical accident of C being older than memory
      managed CPUs that provided the read-only code segment
* so if you get a `char[]` in C you must read the code to find out
    1. its maximum size
    2. whether you can modify it or not
        * might be declared const
        * might be a literal (stored in .text segment so implicitly read-only)
* C silently appends null `\0` to string literals in your code before it stores
  them in the code (.text) segment
    * the null byte is used by C functions to decide when the end of the string
      has been reached so strings constructed dynamically should have it added
      too
* The characters of a literal string are stored in order at contiguous memory
  locations.
* An escape sequence (such as \\ or \") within a string literal counts as a
  single character.
* A null character (represented by the \0 escape sequence) is automatically
  appended to, and marks the end of, each string literal.
* Maximum string literal length:
    * ANSI compatibility requires a compiler to accept up to 509 characters in a string literal after concatenation
    * most compilers go much higher e.g. MSVCC compiler:
        > While an individual quoted string cannot be longer than 2048 bytes, a
        > string literal of roughly 65535 bytes can be constructed by
        > concatenating strings.

```c
// allowcate an array of 6 chars in the stack segment
// can be modified
char m2[6] = "hello"; // 6 is correct size, allows for null byte

// allocate an array of 6 chars in the code segment (.text segment)
// is type char[] but will segfault if you try to modify
char *m2 = "hello";
```

## extern

*
* a way of creating global variables i.e. variables that are _external_ to all functions
* storage is allocated for the variable only once
* the variable is declared outside of any function (even main)
* functions that wnat to reference the global have to declare it (all variables in C must be declared before us) via the extern keyword
* you can omit the `extern` keyword iff
    * the external variable is declared higher up in the same source file
* if you want to reference globals from other files you _Must_ use `extern`
* global variables are usually declared in header files


int dostuff() { } // empty argument list provides backwards compatibility, turns off argument checking, bad :-(
int dostuff(void) { } // more explicit about "no args", preferred option

Variable definition = the place where the variable is created a.k.a. storage is allocated
Variable declaration = the nature of the variable is declared but no storage is allocated
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

## Preprocessor directives

* define is a simple text replacement
* the replacement text can be _any_ series of characters
* Note: no semicolon at the end of the line (it is removed before the compiler sees the file)

```
#define FOO anything
```

## Functions

```
return-type function-name(param1-type param1-name, ...) {
    variable-declarations
    statements
}

parameter == formal argument == the variable named in the parameterized list of a function definition
arugment == actual argument == the value actually passed to the function when invoked
```

## IO in C

* C has a simple model of IO: text input and output is always represented as streams not matter where it is coming from or going to
* a stream is a collection of lines
* each line is 0+ characters terminated by a newline


```c
c = getchar() // reads the next character (byte) from an input stream
putchar(c) // writes a single character (byte) to an output stream
```

* getchar()
    * expands to `getc(stdin)`
    * returns an "unsigned char" converted to an int
    * returns a special `EOF` value when it reaches the end of the stream or an error occurs
        * EOF is a "negative integral constant"
        * EOF is an integer defined in stdio.h
        * the specific numeric value does not matter as
          long as it is not the same as any possible
          character

unsigned char

* signed and unsigned char comes from using char to store numbers
    * usually you can ignore the "signedness" of chars if you are just putting text in them

> The difference between signed char and unsigned char is as you'd expect. On
> most platforms, signed char will be an 8-bit two's complement number ranging
> from -128 to 127, and unsigned char will be an 8-bit unsigned integer (0 to
> 255). Note the standard does NOT require that char types have 8 bits, only
> that sizeof(char) return 1. You can get at the number of bits in a char with
> CHAR_BIT in limits.h. There are few if any platforms today where this will be
> something other than 8, though.

http://www.arm.linux.org.uk/docs/faqs/signedchar.php

>  The C standards do say that "char" may either be a "signed char" or "unsigned char" and it is up to the compilers implementation or the platform which is followed.


Assignment is an expression and returns the value of the LHS after the assignment

```c
x = do_thing() // returns the value of x after do_thing has been evaluated and stored in x
```

## type conversions


* C _arithmetic operators_ cannot work on a mix of int and float so will convert int to float
    * consequences
        * integer division truncates

```
int operator int // no conversion, operator operates directly on integers
float operator float // no conversion, operator operates directly on floats
int operator float // int silently converted to float before operation, operator operates directly on floats
```

# Generic pointers

```
+ allows you to avoid type checking
+ can point to anything
- must be cast to another pointer before it can be dereferenced
    - the programmer must keep track of this
- cannot do pointer arithmetic on them
```
## Variable names

* Only first 31 chars matter to the compiler
* `a-zA-Z0-9_` only. Also must not start with digit

## Arrays

* are homogenous (all items must be same type)
* 0 based index
* must be sized when created
* defined by giving
    1. type
    2. name
    3. number of elements
* The name of the array is a pointer to its first element.
    * `=>` arrays are always passed to functions by reference
* Used to implement strings in C (string is an array of characters terminated by `\0` (null byte))

TODO: play with passing an array by value

```c
// type name[num-elements]
int ages[11];
```

### Strings

* C has no string variable type
* A string literal is an array of characters
* The name of an array is a pointer to the first element in it.
    * => the name of a string literal is _a pointer_ to a character
    * => You can assign string literals to variables that point to `char`
* In C a string literal evaluates to an an array of
  length n+1 (n = num chars in string, +1 for the null
  byte)
* string literals just a shorthand way of making an array
  of characters

```c
// equivalent
char foo[6] = { 'h', 'e', 'l', 'l', 'o', '\0' };
char* foo = "hello";

char *p; // create a pointer to a char
p = "hi there" // "hi there" evaluates to a memory address that is stored in p
```


## Preprocessor

`#define`

* Assocaiates symbolic names with a constant
* gets substituted in before compilation
* is a simple text replacement
    * Don't use it for types - use `typedef` instead becasue `typedef` knows how
      to handle comma separated variable declarations

# IO functions

## `scanf`

* Reads a string from STDIN, optionally parses bits out of it  and stores stuff
  in a memory location.
* First arg is a format specifier.
* Second arg is the _memory address_ that the data should be put.

```
scanf( "%s", &ages[3] );
```

# getting memory address of a variable

* An `&` preceding a variable refers to the address of that variable
* `&foo` is the address in memory that the variable foo refers to.


# Storage classes

* storage class determines
    1. the part of memory where memory is allocated for the variable.
    2. how long the allocation continues to exist
1. `auto`
    * there is an `auto` keyword but it is not commonly used
    * is the default storage class - if you omit a storage class when declaring a
      variable then `auto` is used.
    * `auto` variables are allocated when you enter a block and deallocated
      when you exit it.
    * access restricted to the block in which they are declared as well as any nested blocks
    * also know as local variables
2. `static`
    * like `auto` they are scoped to the function they are declared in
    * but they are only allocated once and retain their value after the function
      has exited.
    * good for any state that is
        * should not be available to any other function
        * needs to be tracked across invocations of that function
        * they are sort of a global that is restricted to just that function.
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
    * `static` set on a global variable _or_ function restricts access to it to
      just the file it was defined in. It reads as "make this global but only to
      this file".
        * it is a primitive form or access control
    * functions are _external_ storage class by default but can be made static.
3. `register`
    * tells compiler you would prefer if this variable was stored in registers not in main memory
    * a way of suggesting to the compiler that we will need fast access to this
      variable - implementations do not garuantee they will do it.
    * they have the same scope as automatic variables (function local)
4. `extern`
    * declare without defining
    * The compiler converts a single `.c` file into a `.o` file. `extern` tells
      the compiler "hey don't worry about resolving this variable from the
      source file you are currently compiling - the linker will find the
      definition for you.
        * extern variables are compiled without the variable being resolved - it is
        resolved by the linking process.
    ```c
    extern int foo2; // alwasy a declaration (no memory allocated
    int foo3 = 12; // always a definition (memory is allocated)
    int foo1; // could either be definition or declaration depending on compiler
    ```
    * if used within a function it tells the compiler that you want to use the
      _global_ named variable
    * creates global variables
        * they last for the lifetime of the program
    * function declarations are implicitly extern
        * functions in C are external by default (available to all source files)
    * there is one box in memory and all source files can use it.
    * `extern int foo; this keyword tells the compiler that "hey this integer
      called foo already has storage allocated for it by some other file so
      don't make a new box".
    * to make a global
        * just declare the variable as normal in `file1.c` - it needs to be
          declared outside the scope of any function
        * reference it in any funcion in `file2.c` using `extern`
        * the variable name and type have to match obviously
    * the `extern` keyword is not used to make variables in the external area -
      you do that by delcaring them outside any functions.
       * external variables are created by the position of the declaration in
         the file.
       * all code following the declation can access that variable no matter
         what file it is in.
    * `extern` within a function:
        * `extern` is used to tell the compiler that you want to reference
          global variables from within your function.

## Declaration vs definition vs instantiation

* A definition
    * is a special kind of declaration that creates an object.
    * specifies the type of the object
    * reserves storage for it
    * is used to create the object
    * can occur only once
* A declaration
    * indicates a name and allows you to refer to an object created
    * can occur many times
    * describes the type of an object
    * is used to refer to an object which may be _defined_ elsewhere.
    * is like a customs declaration - "this is the name and how big it is but the
      acutal thing is somewhere else"
    * since you are not allocating memory in a declaration you don't have to
    * tell the compiler exactly how big it is

* Variables in C are _boxes_ in memory.
* The name is the identifier that your code will use to refer to the box.
* The type tells the compiler
    1. how big the box will be
    2. what kind of values can legally be in it.
* You can get access to the location of the box using `&` for _any_ variable


### Terminology

* Define _declare_
    * tell the compiler about the variable
    * tell the compiler the name and _type_ of the variable so it knows how much
      space to allocate and what values are legal.

* Define _define_
    * have the compiler allocate storage.
    * in the case of variables, the "definition" (or allocation of storage)
      happens at the same time as "declaration" for _automatic_ and _register_
      variables. They are different for _extern_
    * functions in C have a declaration and definition
    * you have to declare (but not define) a function before you use it.
        * there is a lot of duplication between the declaration and definition
          so presumably the declaration is to help the compiler.

* Define _instantiate_
    * declare + set a value on a variable (put a value in the box)
    * instantiation = the process of representing an abstract concept by an instance
    * instance := an example or single occurance of something
    * so i guess an instance is an example of something. `int x = 23` creates a
      box and puts something in it so we have a complete example of a variable.

Aside: C seems to be all about the human helping the compiler not the other way
round.

# Most complete form of variable delcaration

```
{storage-class} {type} {optional derefernce operator}{variable-name/;
```



# Type qualifiers (const, volatile)

* `const`
    * make this thing immutable
    * compiler should complain if you try to assign to this
    * they are usually initialized to get that first value in there
* `volatile`
    * tells compiler not to optimize the variable
    * useful if you are using `setjmp()` and `longjmp()` stuff

# Memory organisation

* Text (or Code) area
    * where the app instructions are put
* Stack area
    * a LIFO queue
    * memory is alloacted and deallocated automatically by the running code
    * function parameters, automatic (local) variables are stored here
* Heap
    * `external` and `static` variables are allocated here

# structs

* items in a struct are called _structure members_

```c
// create structure template (does not allocate any memory)
struct person
{
    int age;
    char *name;
    int scores[5];
}
```
* _structure members_ are not necessairly stored contigiously in memory (so you shouldn't do pointer math with them)
* First you create a structure template (but this does not allocate any memory)
* `.` is the _member selection operator_
* If you have a pointer to a struture you use `->` ?? operator to get at members.

```c
struct person
{
    char name[255];
    int age;
}

typedef struct person PERSON;

struct person eoin = { "Eoin Kelly", 35 };
struct person *perpoi;
perpoi = &eoin;

printf("%s\n", (*perpoi).name); // dereference the pointer and call the member selection operator
printf("%s\n", perpoi->name);   // same thing (cleaner syntax)
```

* (nested) structures can be initialized with `{ ... }` same way (nested) arrays can.
* structures can be nested.

# typedef

* lets you make a synonym for an _existing_ type.

```c
typedef char * STRING;

// These do the same thing
char *s = "hi";
STRING s = "hi";

// it does the right thing with multiple declarations separated by commas:
STING name, comment;

// NB #define would get this wrong
#define STRING char *
STRING a, b, c; // Looks ok but because define is simple text substitution it becomes ...
char *a, b, c; // a is a pointer to a char, b is a char, c is a char
```

* is often used with structures to avoid typing `struct my_thing` every time you
  need to create one.
* by convention alias is UPPERCASE.

# other stuff to revise

enum
unions

