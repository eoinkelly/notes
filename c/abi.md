# ABI

Sources

* https://www.youtube.com/watch?v=By7b19YIv8Q
* https://thephd.dev/binary-banshees-digital-demons-abi-c-c++-help-me-god-please
* https://thephd.dev/to-save-c-we-must-save-abi-fixing-c-function-abi

Overview

* An ABI is how your object and types look to the compiler
* it includes:
    * size, number and order of data members in a type
    * size, number and order of virtual functions
    * number an order of function parameters
    * function return types
        * return type of a function is not baked into the mangled name created by C++

I have a function `int do_thing()` which gets mangled to `Z9_do_thing` (or whatever, this is just an example)
I compile my library and ship the compiled library and header file for others to use

... time passes ...

I change do_thing to `double do_thing()` which is still mangled to `Z9_do_thing`
I compile my library and ship the compiled lib +  header file

What if a consumer built their application to dynamically link to my lib with the old header
But the user is running the app on a system with the new version of my lib.
A bad time.



An ABI includes


* how a function calls another function
* how the stack isetup
* the scope of variables
* how a data structures bits and bytes are traversed
* the position, order, and layout of members in a struct/class;
* the argument types and return type of single function (C++-only: and any relevant overloaded functions);
* the “special members” on a given class (C++-only);
* the hierarchy and ordering of virtual functions (C++-only);
* and more.

## The C ABI

* not a formal documented thing but still a real constraint

> Well, for C, it means that changing anything - literally anything - is an ABI
> break. Change your typedef to be a bigger integer? ABI break. Fix your time_t
> structure to handle time with 64-bit numbers

> despite it being common practice amongst libraries like musl libc and glibc to
> provide weak aliases and other forms of implementation-specific indirection over
> functions to prevent ABI problems (and allow for safe overriding in select
> circumstances), the standard itself is bound by the simplest possible
> implementation of the C Standard Library. Because there is no standard-mandated
> solution for name aliases / indirection in C, or ways to provide name-mangling
> on your own, someone can create a POSIX implementation that takes all of the
> names in the C Standard very literally. All of the names — strcpy, strcat,
> memset, gets_s, what have you — appears exactly in the system’s globally shared
> symbol table. That means that even if a specific application wanted to, say,
> request a printf that’s capable of printing int128_t or has special format
> modifiers for certain structs, that printf has to be globally upgraded and
> agreed upon by every single program in the system that uses the C library.