# typedef

* creates aliases for types
    * does NOT _create_ new types in any way!
    * there are no new semantics
    * it is a bit like a fancy `#define`
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

// TODO: syntax seems different for function pointers?
typedef int (*funky)(char *, char *)

// Examples from standard library
//
// * size_t
// * ptrdiff_t
```

