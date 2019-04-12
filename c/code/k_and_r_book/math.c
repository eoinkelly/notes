#include <stdio.h>
#include <math.h>

int power(int base, int n)
{
  int c = 1, output = base;

  while(c < n) {
    output = output * base;
    ++c;
  }

  return output;
}

int function_name() {
  printf("hello");
}

int main(int argc, char *argv[])
{

  // C has no built-in exponention operator e.g. x**y
  // but pow() from the stdlib does the job
  printf("Exponentaion: %lf\n", pow(2,5));

  printf("power: %d\n", power(2,5));
}

