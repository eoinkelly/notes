# Globals

Any variable declared outside of a function or method, class @interface or
struct declaration is a global variable

- Globals are special
    - they are initialized to "zero" i.e. for regular pointer types they are
      initialized to NULL and for ObjC pointers they are intialized to 'nil'
    - can be assigned a value when declared but it must be something the
      compiler can compute at compile time i.e. no function calls
- Local variables
    - are not magically initialized so should always be initialized before use

# Defining constants in Objective C

`const` _ is a storage specifier _ added to a variable declaration it says that
the compiler should expect variable is read-only

static and extern are C storage classes

`static` _ outside a function: _ `static` set on a global variable _or_ function
restricts access to it to just the file it was defined in. It reads as "make
this global but only to this file". _ it is a primitive form or access control _
means that this constant will only be available in the file it is defined in _
within a function _ they are only allocated once and retain their value after
the function that allocated them has exited. \* can be used to create singletons
from a factory method

`extern` * `extern` keyword is *not* used to make variables in the external
area - you do that by delcaring them outside any functions. * outside a function
_ used to tell the compiler that you want to use the *global* copy of the named
variable _ extern variables they last for the lifetime of the program _ there is
one box in memory and all source files can use it. _ within a function \*
`extern` is used to tell the compiler that you want to reference global
variables from within your function.

```
// allocate space for an integer and get the compiler to yell if we try to change it.
const int MAX_AGE = 10;
```

## 5 methods of making constants

1. #define macro
    -   - debuggers do not know about them (they become their value at compile
          time)
    - not recommended for this reason
2. const
    - used on its own to create local constants
3. static const
    - best for file specific or class specific constants
4. extern const
    - you _must_ use this when exporting a constant in a header
5. enum
    -   - can only define integer constants
    - NS_ENUM and NS_OPTIONS are probably better

Questions to use to decide how to make the constant:

1. Is the constant for all classes or just one class?
2. Is the constant exposed as public by the class or part of its private
   implementation?

## Constant is for just one class and is internal to the class

Declare it as `static const` at the top of the `.m` file

```objc
// declare and define in one line
static NSString *const MY_CONST = @"foo";
// or
static NSString * const MY_CONST = @"foo";
```

## Constant is for one class but is part of public interface of that class

Declare it as `extern` in the `.h` file and define it at the top of the `.m`
file.

```objc
// .h file
extern NSString *const MY_CONST;

// .m file
NSString *const MY_CONST = @"foo";
```
