#include <stdio.h>
#include <stdlib.h>

int main(int arc, char *argv[]) {

  size_t num_bytes = 100;
  int bytes_read;
  char *my_string;

  my_string = (char *) malloc(num_bytes + 1);

  printf("What is your name? ");

  bytes_read = getline(&my_string, &num_bytes, stdin);

  if (bytes_read == -1) {
    puts("There was an error");
    exit(1); // TODO: what exit code is fail?
  }
  printf("%d bytes read\n", bytes_read);
  printf("Hi, %s, it is nice to meet you.\n", my_string);
  exit(EXIT_SUCCESS);
}
