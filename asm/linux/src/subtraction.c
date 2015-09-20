#include <stdio.h>
#include <stdlib.h>

int subber(int x, int y) {
  return 2 * x + y;
}
int main(int argc, char ** argv) {
  int a;
  a = atoi(argv[1]);
  return subber(argc, a);
}

