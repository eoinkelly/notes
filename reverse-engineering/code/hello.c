#include <stdio.h>

int add(int a, int b) {
  return a + b;
}

int main(int argc, char *argv[])
{
  printf("Hello world\n");
  printf("Answer: %d\n", add(4,6));
  return 0;
}
