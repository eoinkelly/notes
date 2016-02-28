#include <stdio.h> // required for printf()
#include <stdlib.h> // required for exit()

// * C99 and C11 implicitly returns 0 from main so you don't ever need an
//   explicit return statement
// * the integer you return from main() will be the value that the shell (or
//   whatever the calling process is) gets back
//
// exit() and _Exit() are stdlib ways of exiting that do other cleanup (close
// streams, unlink temp files) etc.
//
//   $ man 3 exit
int main()
{
  printf("no need to explicitly return 0\n");

  // return(3); // defaults to 0 if no explicit return
  // exit(3); // returns 3 to the shell and does other cleanup
}
