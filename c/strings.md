
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

