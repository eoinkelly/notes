#include <stdio.h>

int max = 12;

void do_things_to_max() {
    extern int max;
    ++max;
    printf("-%d-", max);
}

int main(int argc, char *argv[])
{
  do_things_to_max();
  do_things_to_max();
  do_things_to_max();

  return 0;
}
