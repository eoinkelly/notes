#include <stdio.h>

int main(void)
{
  // *******************************
  // Test: placement of * when declaring a variable
  char* t_1;
  char *t_2;
  char*t_3;
  char* test_1 = "hello";
  char*test_2 = "hello";
  char *test_3 = "hello";
  // conclusion: the location of * does not matter
  // *******************************

  // *******************************
  // Test: string literals are equivalent to null-byte terminated arrays of characters
  char* foo_1 = "hello";
  char foo_2[] = { 'h', 'e', 'l', 'l', 'o', '\0' };

  printf("foo_1: %s", foo_1);
  printf("\n\n");
  printf("foo_1: %p", foo_1);
  printf("\n\n");
  printf("foo_1: %c", foo_1[1]);
  printf("\n\n");

  printf("foo_2: %s", foo_2);
  printf("\n\n");
  printf("foo_2: %p", foo_2);
  printf("\n\n");
  printf("foo_2: %c", foo_2[1]);
  printf("\n\n");

  // *******************************
  // Test: a character pointer can be assigned a string literal
  char* bar_1;
  bar_1 = "hello";

  printf("bar_1: %s", bar_1);
  printf("\n\n");
  // conclusion: it can
  // *******************************

}
