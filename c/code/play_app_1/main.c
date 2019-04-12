#include <stdio.h>
#include <stdlib.h>
#include "user.h"

int main()
{
  user_t eoin = {.first_name = "Eoin", .last_name = "Kelly", .age = 38};

  // user_get_full_name(...) will malloc a string and return a pointer. It is
  // our responsibility to clean it up.
  char* full_name = user_get_full_name(&eoin);

  printf("First name: %s\n", eoin.first_name);
  printf("Full name: %s\n", full_name);

  free(full_name);
}
