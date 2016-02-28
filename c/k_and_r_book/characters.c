#include <stdio.h>
#include <math.h>

int main(int argc, char *argv[])
{
  if ('A' == 65) {
    printf("A == 65\n");
  }

  // C has no problem with you comparing an int and a char
  int x = 65;
  char c = 'A';

  if (x == c) {
    printf("A == 65\n");
  }

  // use c-'0' trick to get the numeric value of a character c in range '0' - '9'
  char num_char = '4';
  printf("Character '%c' as number is %d\n", num_char, (num_char - '0'));

  printf("Exponentaion: %lf\n", pow(2,5));

  return 0;
}
