#include <stdio.h>

/*
 * The book says:
 * "environ points to a NULL-terminated list of pointers to null-terminated strings"
 *
 * let me see if we can work this out from the symbols:
 *
 * * environ is a symbol that is a pointer to a pointer to a character variable
 * * "pointer to a character variable" is indistinguisable in C from "pointer
 *   to an null terminated array of characters"
 *
 * so environ is a symbol that is a pointer to an array of characters"
 *
 */
extern char** environ;

int main(int argc, char *argv[])
{
  printf("Here is the environment:\n");

  int len = sizeof(environ) / sizeof(char);
  printf("Length is %i\n", len);

  for (int i = 0; i < len; ++i) {
    printf("%s\n", environ[i]);
  }
  return 0;
}
