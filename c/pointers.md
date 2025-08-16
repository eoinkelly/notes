# Pointers

## K&R Chapter 5: Pointers & Arrays

- "pointer to OTHER_TYPE" is a data type in C
- there is no such thing as "a pointer" - it is always "a pointer to X"
    - every pointer points to a specific data type
- pointers take up a fixed amount of storage which varies by system (8 bytes on
  my laptop)

```c
int *x; // reads two ways:
// "x" is going to be a pointer to an int
// "*x" is going to be an int
```

### & operator

- only applies to objects in memory (variables and arrays) - it cannot be used
  with
    - constants
    - expressions
    - register variables

- can be used to get a pointer for an existing data type

```c
int x = 12; // create automatic variable x
int *xp = &x; // create a pointer to x
```

- C passes all arguments to functions by value but pointers allow you to work
  around this
    - it is clear in the syntax when you are manipulating a pointer vs
      manipulating a value directly
        - => so pointers are not the same as "pass by reference" where that
          difference is not visible
- pointers can be used to approximate "mulitple return values"
    - you can pass "in/out" pointer that the funciton can fill and it can use
      its return value to signal something else e.g. failure, end of file etc.

### Generic pointer

- is the exception to the "every pointer points to a single specific type" rule
- cannot be dereferenced itself ???
- In _very_ old-school C `char *` was the pointer type for a generic pointer
- In everything since `void *` is the correct type for a generic

## Generic pointers

```
+ allows you to avoid type checking
+ can point to anything
- must be cast to another pointer before it can be dereferenced
    - the programmer must keep track of this
- cannot do pointer arithmetic on them
```

- any pointer can be cast to `void *` and back again without loss of information

# getting memory address of a variable

- An `&` preceding a variable refers to the address of that variable
- `&foo` is the address in memory that the variable foo refers to.
- is not required for function names and array names

## function pointers

- a function is not a variable but ou can define a pointer to it
- you do not need the `&` when taking the address of a function - you just need
  its name (same as arrays)

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

- -- it doesn't make it obvious that you need to lookup the creation of a
  pointer not definition of a function `pfoo`
- ++ you can seemingly ignore whether what you have is a function pointer or a
  real function
