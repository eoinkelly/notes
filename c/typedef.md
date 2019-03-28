# typedef

* creates aliases for types
    * does NOT _create_ new types in any way!
    * there are no new semantics
    * it is a bit like a fancy `#define`
* lets you make a synonym for an _existing_ type.
* uses
    * make program easier to read especially with complicated structures
    * parameterise the program against portability problems
        * example: if you typedef the platform `int` to `size_t` then you can handle portability issues by adjusting the typedef
* K&R uses a convention where new types have uppercase names
    * seems to be followed sometimes but not always in postgres codebase
* it is handy for structs to avoid having to type the `struct struct_label_name` every time you want to use it as a type

```c
// typedef existing_type new_name;
typedef char * String;
typedef int Coordinate;

// this creates funky as an alias for a function which takes two `char *` and
// returns an int. It is just an alias and does not allocate any storage for a
// pointer!
typedef int (*funky)(char *, char *);

// allocate storage for a function pointer
funky pf;

// Examples from standard library
//
// * size_t
// * ptrdiff_t

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

