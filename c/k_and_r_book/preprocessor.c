#include <stdio.h>

// preprocessor directives can be *anything*
// also this is a *terrible* idea :-)
#define FOO void func2() { printf("hi 2\n"); }

void func1() {
  printf("hi\n");
}

FOO

int main() {
  func1();
  func2();
}
