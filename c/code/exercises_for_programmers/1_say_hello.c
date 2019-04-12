#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_STR_LEN 100

int main()
{
  char* user_input = (char*)malloc(MAX_STR_LEN);

  printf("What is your name? ");

  size_t num_chars = MAX_STR_LEN - 1;
  if (getline(&user_input, &num_chars, stdin) <= 0) { exit(EXIT_FAILURE); }

  // overwrite the last \n with \0
  size_t last_char_idx = strnlen(user_input, MAX_STR_LEN) - 1;
  user_input[last_char_idx] = '\0';

  printf("Hi %s, it is nice to meet you.\n", user_input);

  free(user_input);
  exit(EXIT_SUCCESS);
}
