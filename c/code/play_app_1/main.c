#include <stdio.h>
#include "user.h"

int main(int argc, char *argv[])
{
  printf("Starting play_app_1\n");
  user_t eoin = { .first_name = "Eoin", .last_name = "Kelly", .age = 38 };
  printf("First name: %s\n", eoin.first_name );
  printf("Full name: %s\n", user_get_full_name(&eoin));
  return 0;
}
