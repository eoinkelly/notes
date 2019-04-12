

CHAP 7

#### Misc short tips

* Since C99 You don't need an explicitly `return 0;` from `main` - the spec says that `main` will do it for you.
* You don't have to declare all your variables _at the top of a function_ since ANSI C89 - you still have to declare functions but can do them in a "just in time" style.
* You can declare variables in the head of a `for` loop since C99
* You can rely on the compiler if you declare a variable within a loop - it won't _actually_ deallocate and re-allocate space each time through (even though that is what seems to happen

#### You can set array size at runtime - you don't need to use malloc

```c
int num_things = atoi(argv[1]);

// stack allocated array of things of size num_things
struct thing things[num_things];
```

#### How to handle a void pointer

```c
int do_stuff(void *in) {
    // this effectively casts the void pointer to a pointer to a `struct thing`
    struct thing *my_thing = in;
}
```

#### Author things we should cast less

* casts are a way of overriding the type system so should be used sparingly

```c
// Times when a cast is still required:

// this will return an integer
int a = 33;
int b = 4;

// either of these work to force the computation to return a floating point value
// author perfers the 0.0 version
double c1 = a / b + 0.0;
double c2 = a / (double)b;

// Array indices must be integers so you have to cast
// Note that this assumes you are confident that the floating point type
// contains an integer value
some_ary[c1]; // compiler error
some_ary[(int)c1]; // compiler error
```

#### Avoide "assembly style" C when possible

```
goto

switch
    case
    default
    break

break
continue
```

* The above keywords are part of an "assembly style" of C programming which 1) is optional and 2) you should probably avoid when possible.
* `switch` statements are better implemented as a collection of `if() else if() ...`
* if you have error handling at the end of a function which has to happen no matter what the error was then `goto` can be a clean way of implementing that (it avoids duplication), even in 2019

#### Don't use 'float', use 'double' or 'long double'

* `float` 32 bit float can store only 7 significant digits, errors are frequent, don't use it.
* `double` 64 bit floats can store 15 significant digits, less error prone
* `long double` can be different lengths but is at least 64 bits
    * https://en.wikipedia.org/wiki/Long_double
    * Under visual c++ `long double` defaults to just being an alias for `double`
    * on x86 it is often an 80 bit extended precision type (which may be padded out to 96 or 128 bits)
    * On ARM `long double` _seems_ to be an alias for `double` (I didn't research this super carefully so I'm not sure which ARM architectures that applies to)
        * http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0491c/Babfcgfc.html
    * on some architectures (e.g Sparcv9) it is implemented as exactly two doubles summed (note this does not to the IEEE floating point standard)

#### Don't use unsigned ints

> In most comparisons of signed and unsigned integers C will force the signed int to be unsigned

* This is very ununtuitive and can cause bugs
* Author believes that the many variations in int types C gives you leads to "micromanaging types"

### Use strtol/strtod instead of atoi/atof to convert strings to numbers

* atoi/atof don't do any error checking to see if the string can be successfully converted

* variants
    * strtof(str_to_convert, first_unconvertable_char) // string to float
    * strtod(str_to_convert, first_unconvertable_char) // string to double
    * strtol(str_to_convert, first_unconvertable_char, base) // string to long int
    * strtoll(str_to_convert, first_unconvertable_char, base) // string to long long int

```c
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char* argv[])
{
  char* end = NULL;
  char* maybe_num = "123a";
  int base = 10;

  int num = strtol(maybe_num, &end, base);

  if (*end != '\0') {
    // there was an error if we expected the whole string to be a number and
    // end is not now pointing at the terminating null character
    printf("error. last char: %c\n", *end);
    return -1;
  }
  printf("num: %d\n", num);
  return 0;
}
```

#### The many uses of NaN

* NaN can be used as a null terminator for a variable length list of floats


#### Zeroing out automatic memory

* You have to use memset if the array length is decided at runtime, otherwise you can use designated initializers & friends


#### const

* const is a literary device not a lock on the data
