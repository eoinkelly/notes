#include <stdio.h>

int main() {
  long counter;

  // getchar() != EOF returns 1|0 which C interprets as truthy|falsy

  // option 1:
  // this no {} for loop is fucking gross
  for(counter = 0; getchar() != EOF; counter++)
    ;

  // variation
  for(counter = 0; getchar() != EOF; counter++) { }

  // option 2:
  while(getchar() != EOF) {
    ++counter;
  }


  printf("%ld", counter);
}
