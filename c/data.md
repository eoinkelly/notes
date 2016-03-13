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
## Variable names

* Only first 31 chars matter to the compiler
* `a-zA-Z0-9_` only. Also must not start with digit


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

## type conversions


* C _arithmetic operators_ cannot work on a mix of int and float so will convert int to float
    * consequences
        * integer division truncates

```
int operator int // no conversion, operator operates directly on integers
float operator float // no conversion, operator operates directly on floats
int operator float // int silently converted to float before operation, operator operates directly on floats
```
