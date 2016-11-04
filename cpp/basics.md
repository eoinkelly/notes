# C++ basics

Variables are declared by

    VAR_TYPE VAR_NAME;

Variables are initialized by

    VAR_TYPE VAR_NAME = VAR_INITIAL_VALUE;

In C/C++ uninitalized variables are "undefined" according to the spec. In
practice you get a random value, which is usually 0. But it's not guaranteed to
be 0.

Pro tip: Always initialize variables when you declare them!

Functions are declared as

    RETRUN_TYPE FUNC_NAME (ARG1_TYPE ARG1_NAME, ...)
    {
        FUNC_BODY
    }

notice

* style: space between func name and opening paren
* style: curly braces on new line

The compiler must see variable or function declaration before it can be used

## Aside: Reading C type declarations

Sources

* http://ieng9.ucsd.edu/~cs30x/rt_lt.rule.html
* http://unixwiz.net/techtips/reading-cdecl.html

A C declaration has

1. exactly one basic type (char, short, int, long, float etc.)
1. exactly one identifier
1. 0 to many "derived types"

The basic type is always on the far left of the expression

There are exactly three kinds of derived type in C

1. `*` = "pointer **to**"
2. `()` = "function **returning**"
    * there may also be grouping parentheses in the expression but they will not be an open followed immediately by a close
    * `(ARG_TYPES)` = "function expecting ARG_TYPES and **returning**"
3. `[]` = "array (unsized) **of**"
    * `[X]` = "array (size X) of

A derived type always modifies something that **follows** so when converting to english you need the "of" or "to" or "returning"

C has precedence for derived types: "array of" and "function returning" have higher precedence than "pointer to" i.e. the things on the right of an identifier bind tighter

Always start with the variable name and end with the basic type e.g. `char * const argv[]` begins converting to english as:

    argv is ... char

Then follow the _go right when you can, and go left when you must_ rule to fill in the middle

    argv is ... char
    argv is array (unsized) of ... char
    argv is array (unsized) of const ... char
    argv is array (unsized) of const pointer to char

Note: not all combinations of derived types are allowed by the language e.g.

* C function cannot return an array (it can return a pointer to an array)
* C function cannot return an function (it can return a pointer to a function)
* Cannot have arrays of functions (can only have arrays of pointers to functions)
* Only left-most [] in multi-dimensional array can be undimensioned e.g.
    ```c
    int things[][] = {}; // illegal
    int things[][3] = {}; // ok
    ```
* void type is restricted
    * `void` is a special pseudo-type that is only legal to have a "pointer to void" or "function returning void". You cannot have an array of void or a variable of just type `void`
    ```c
    void foo;       // illegal: "foo is a void"
    void foo[3];    // illegal: "foo is an array (size 3) of void"
    void * foo;     // ok: "foo is a pointer to void"
    void foo();     // ok "foo is a function returning void"
    ```

Abstract declarators

There are two places in C where you can have type delarations without an identifier:

1. casts
1. arguments to `sizeof`

The same rules apply for converting them to english except you first have to find where the identifier would go (if there was one)

> find where the variable name would go, then treat it like a normal declaration

The rules for where the identifier go are

* to the right of all the "pointer to" derived type tokens
* to the left of all "array of" derived type tokens
* to the left of all "function returning" derived type tokens
* inside all the grouping parentheses

```c
int (*(*)())()
// with imaginary identifer XXXX added: int (*(*XXXX)())()
// then follow the normal rules
```

# namespaces

```cpp
namespace Foo
{
    int x = 12;
}
using namespace std;
using namespace Foo;

int main ()
{
    std::cout << Foo::x; // ignoring the using ...
    cout << x;           // taking advantage of namespaces
}
```

* a namespace is a container for a set of identifiers
* C++ has namespaces to prevent having to prefix function names with package names e.g. `mything_do_stuff()`
* `std` is the prefix for the built-in C++ function namespace
* `using` keyword
    * introduces a namespace into the current declarative region
    * ++ saves typing
    * -- you can introduce multiple namespaces into the current declarative region and if they have clashing identifiers you will be sad e.g.
        ```cpp
        namespace Foo {
            int x = 12;
        }
        namespace Bar {
            int x = 13
        }
        using namespace Foo;
        using namespace Bar;
        cout << x // what will it be?
        ```
* never put `using namespace ...` declarations in header files
    * it forces every file which includes the header to be in that namespace - rude.


## pre-increment and post-increment operators

The difference between pre and post is whether it returns the old or the new value

```cpp
int i = 1
cout << i++; // incements i and returns old value (1)

int j = 1
cout << ++j; // incements i and returns new value (2)
```

## strings

* In C
    * `char`
        * would be better named `byte` because it represents a 1 byte value
        * a string is an array of `char`
        * a string lives between the pointer to the first element in the array and a null byte to indicate the end
        * Available string functions:
            * strcmp
            * strcat
            * others?
        * all characters are assumed to be one byte - the standard string functions depend on this
    * `wchar_t`
        * wide character types were added in C99
        * 16 bit character (so it does not fit all possible unicode chars)
        * `L"some string"` shorthand to make wide-char literals
        * -- does not have fixed size across compilers
        * -- some C functions still depend on `char` so you can't _just_ use `wchar_t`
        * -- the specs don't mandate a character size (in bits) or encoding
        * functions which work on wide chars
            * wcscmp
            * wcscat
        * usage is erratic -
    * `char16_t`, `char32_t`
        * defined in recent(???) C & C++ standards
        * they have a fixed, predictable size unlike `wchar_t`

In C++

* `std::string`
    * encapsulates a standard C string (array of `char`)
    * `#include <string>;` is required to use it
* `std::wstring`
    * encapsulates the C `wchar_t` type
* `std::u16string`,  `std::u32string`
    * these wrap their underlying C character types
    * not well supported TODO: is that still true?
    * arkward to use if you have to interact with legacy code or third party libs

You can put UTF-8 data in a `std::string` but:

1. length(), size() return byte lengths not char lengths
1. indexes within the string refer to bytes not chars
1. the iterators step over individual bytes not chars

```
// There are 5 ways of making a C string in C++
char plainString[] = "hello"; // encoded in the source stream using local encoding, whatever that may be.
wchar_t wideString[] = L"hello"; // wide chars, may be UTF-16 or UTF-32 encoding depending on platform/compiler
char utf8String[] = u8"hello"; // encoded in the source stream using UTF-8 encoding, C++11 only
char16_t utf16String[] = u"hello"; // UTF-16 encoding, C++11 only
char32_t utf32String[] = U"hello"; // UTF-32 encoding, C++11 only
```

<http://utfcpp.sourceforge.net/> is recommended a lot for working with UTF-8 in C++

> The original C++ Standard (known as C++98 or C++03) is Unicode agnostic.

> C++11 provides some support for Unicode on core language and library level:
> u8, u, and U character and string literals, char16_t and char32_t character
> types, u16string and u32string library classes, and codecvt support for
> conversions between Unicode encoding forms.

http://www.nubaria.com/en/blog/?p=289

> a UTF-8-encoded string can be regarded as a glorified array of bytes rather
> than as a real sequence of characters. However, this is rarely a problem
> because there are few situations when the boundaries between Unicode code
> points are relevant at all.

> Besides, the concept of what constitutes one individual character is fraught
> with grey areas. There are many scripts (Arabic and Devanagari, for example)
> where letters can be merged together in ligature forms and it may be a bit of
> a moot point whether such ligatures should be considered as one character or
> a sequence of separate characters. In the few cases where we may need to
> iterate through Unicode code points, like for example a word wrap algorithm,
> we can do that through a utility function or class.

## Integer division

The result of division of ints is also an int by truncation e.g.

```
int a = 5
int b = 2
cout << (a / b) // => 2
```

