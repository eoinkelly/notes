# Overview of C

* has no IO built-in - all IO is done by functions (not part of the language)
* has no operators to work with composite data structures
* is not stongly typed but has some type checking
    * you are not allowed to treat pointers and intergers as the same thing
    * you can use an explicit conversion
* Variables can be
    * internal to a function
    * visible to just the source file they are declared in
    * available globally

# History of C

* was an evolution of BCPL an B (both of which were typeless)
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
