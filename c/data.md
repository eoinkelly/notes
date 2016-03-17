
A variable in C has the following properties

1. scope
    * the area of code that it is a valid name
2. storage class
    * determines the lifetime of the storage associated with a name (or identifier)
3. type
    * rules for interpreting the bits in storage to make values
4. linkage
    * whether the same name in another scope refers to the same object/function

Two storage classes

1. automatic
    * declarations within a block are automatic by default or if `auto` storage specifier used
2. static

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
{storage-class} {type} {optional derefernce operator}{variable-name};
```

# Type qualifiers (const, volatile)

* `const`
    * make this thing immutable
    * compiler should complain if you try to assign to this
    * they are usually initialized to get that first value in there
* `volatile`
    * tells compiler not to optimize the variable
    * useful if you are using `setjmp()` and `longjmp()` stuff
        * TODO: why?
* `register` ???

## Variable names

* Only first 31 chars matter to the compiler
* `a-zA-Z0-9_` only. Also must not start with digit

## Data types

* C fundemental data types are: characters, integers (many sizes), floats (many sizes)
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
* in C the `char` type is basically the "single byte" type, it can hold any byte value, not just printable ASCII
    * use `unsigned char` if you are dealing with binary data (see below)
* character constants in C e.g. 'A' (note single quotes for character constant,
  double quotes for string constant) are just another way of writing a small
  integer

### assignment

Assigment associates from right to left so

```c
a = b = c = 0;
// is equivalent to
(a = (b = (c = 0)));
```

Assignment is an expression and returns the value of the LHS after the assignment

```c
x = do_thing() // returns the value of x after do_thing has been evaluated and stored in x

// y will be assigned and the return value of the assignment (same as y) will be used in the if
if (y = some_thing()) { }
```

### trick: '4' - '0' = 4

if `c` is a character representing a digit you can get its numeric value by evaluating
`c - '0'` because numeric characters are stored in order (in ASCII anyway).

## signed and unsigned char

* In C `int` is always signed but whether `char` is signed or not is compiler dependent
* In theory, all integer types except unsigned char and signed char are allowed
  to contain "padding bits" In realtity they usually don't so you can usually use
  `char` to store binary data but using `unsigned char` to store binary data is
  conventional and is probably what I should do.
* you can ignore the "signedness" of chars if you are just putting text in them


> The difference between signed char and unsigned char is as you'd expect. On
> most platforms, signed char will be an 8-bit two's complement number ranging
> from -128 to 127, and unsigned char will be an 8-bit unsigned integer (0 to
> 255). Note the standard does NOT require that char types have 8 bits, only
> that sizeof(char) return 1. You can get at the number of bits in a char with
> CHAR_BIT in limits.h. There are few if any platforms today where this will be
> something other than 8, though.

http://www.arm.linux.org.uk/docs/faqs/signedchar.php

>  The C standards do say that by default `char` may either be a `signed char`
>  or `unsigned char` and it is up to the compilers implementation or the
>  platform which is followed.

http://stackoverflow.com/questions/13642381/c-c-why-to-use-unsigned-char-for-binary-data

> In C the unsigned char data type is the only data type that has all the
> following three properties simultaneously
>
> * it has no padding bits, that it where all storage bits contribute to the
>   value of the data
> * no bitwise operation starting from a value of that type, when converted
>   back into that type, can produce overflow, trap representations or
>   undefined behavior
> * it may alias other data types without violating the "aliasing rules", that
>   is that access to the same data through a pointer that is typed differently
>   will be guaranteed to see all modifications
>
> if these are the properties of a "binary" data type you are looking for, you
> definitively should use unsigned char
