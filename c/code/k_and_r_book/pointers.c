#include <stdio.h>
#include <stdbool.h>
#include <ctype.h>

void swap(int *ap, int *bp) {
  int temp = *ap;
  *ap = *bp;
  *bp = temp;
}

char getch() {
  return 'x';
}

void ungetch(char c) {
}

// *pn is used as an inout variable - the result will be put in it
int getint(int *pn) {
  int c, sign;

  // keep eating characters until we encounter something that is now whitespace ...
  while (isspace(c = getch())) {}

  // ... then check that the character is one that we are interested in:
  // digits
  // +
  // -
  // EOF (aka -1)
  //
  // anything lese means this string is no longer convertable to a number
  if (!isdigit(c) && c != EOF && c != '+' && c != '-') {
    ungetch(c);
    return 0;
  }

  sign = (c == '-') ? -1 : 1;

  if (c == '+' || c == '-') { c = getch(); }

  for (*pn = 0; isdigit(c); c = getch()) {
    *pn = 10 * *pn + (c - '0');
  }

  *pn *= sign;

  if (c != EOF) {
    ungetch(c);
  }

  return c;
}

int main() {

  // All results on Macbook Air
  printf("%ld\n", sizeof(void *)); // 8

  printf("%ld\n", sizeof(char)); // 1
  printf("%ld\n", sizeof(char *)); // 8

  printf("%ld\n", sizeof(int)); // 4
  printf("%ld\n", sizeof(int *)); // 8

  printf("EOF: %d\n", EOF); // -1

  int a = 2, b = 4;
  printf("a: %d\n", a);
  printf("b: %d\n", b);
  swap(&a, &b);
  printf("a: %d\n", a);
  printf("b: %d\n", b);
}
