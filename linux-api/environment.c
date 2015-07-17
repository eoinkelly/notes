#include <stdio.h>

/*
 * The book says:
 * "environ points to a NULL-terminated list of pointers to null-terminated strings"
 *
 * let me see if we can work this out from the symbols:
 *
 *    environ is a symbol that is a pointer to a pointer to a character variable
 *    "pointer to a character variable" is the same as "pointer to an array of characters"
 * so environ is a symbol that is a pointer to an array of characters"
 *
 */
extern char** environ;

int main(int argc, char *argv[])
{

  printf("hi\n");
  return 0;
}
