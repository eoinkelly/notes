#include <stdio.h>

int blah(int *a, int *b)
{
  return *a + *b;
}

int main(void)
{
  // declare fp is a pointer to a function which takes two pointers (to
  // ints) and returns an int. Also allocate storage for the pointer
  int (*funcPointer)(int *, int *);

  // intPtrsToInt is now an alias for a function that takes two pointrs to ints and returns an int
  // it it a type _alias_, it does not have storage alloacted! (unlike funcPointer above)
  typedef int (*intPtrsToInt)(int *, int *);
  intPtrsToInt deffy; // this allocates storage


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
