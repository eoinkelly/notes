#include <stdio.h>

int main() {
  int c;
  printf("EOF: %d\n", EOF);
  printf("%d\n", (getchar() != EOF));
  // getchar() does not return char
  // it returns an int because it has to encompass EOF as well as any legal character value
  // () are required because != has higher precedence than = so would
  /* while((c = getchar()) != EOF) { */
  /*   putchar(c); */
  /*   c = getchar(); */
  /* } */
}
