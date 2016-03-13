# Pointers


## Generic pointers

```
+ allows you to avoid type checking
+ can point to anything
- must be cast to another pointer before it can be dereferenced
    - the programmer must keep track of this
- cannot do pointer arithmetic on them
```

* any pointer can be cast to `void *` and back again without loss of information

# getting memory address of a variable

* An `&` preceding a variable refers to the address of that variable
* `&foo` is the address in memory that the variable foo refers to.
* is not required for function names and array names

## function pointers

* a function is not a variable but ou can define a pointer to it
* you do not need the `&` when taking the address of a function - you just need its name (same as arrays)

```c
#include <stdio.h>

int blah(int *a, int *b)
{
  return *a + *b;
}

int main(void)
{
  // declare funcPointer is a pointer to a function which takes two pointers
  // (to ints) and returns an int. Also allocate storage for the pointer
  int (*funcPointer)(int *, int *);

  // intPtrsToInt is now an alias for a function that takes two pointrs to ints and returns an int
  // it it a type _alias_, it does not have storage alloacted! (unlike funcPointer above)
  typedef int (*intPtrsToInt)(int *, int *);
  intPtrsToInt deffy; // <-- this allocates storage

  int a = 3, b = 4;
  int *pa = &a;
  int *pb = &b;

  funcPointer = blah;
  deffy = blah;

  printf("%d\n", (*funcPointer)(pa, pb)); // outputs "7"
  printf("%d\n", (*deffy)(pa, pb)); // outputs "7"

  // using "modern syntax"
  printf("%d\n", funcPointer(pa, pb)); // outputs "7"
  printf("%d\n", deffy(pa, pb)); // outputs "7"
}
```

You don't have to dereference the pointer to use it as a function name e.g.

```
int bar(int a, int b)
{
    return a + b;
}

int (*pfoo)(int, int) = bar;

(*pfoo)(1, 2);
// or
pfoo(1, 2);
```

* -- it doesn't make it obvious that you need to lookup the creation of a pointer not definition of a function `pfoo`
* ++ you can seemingly ignore whether what you have is a function pointer or a real function
