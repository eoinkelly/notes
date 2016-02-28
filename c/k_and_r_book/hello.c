#include <stdio.h>

// enable C89
// #define __STDC_VERSION__ 199409L /* c89 was revised in 1994 */
// $ cc -std=c89 hello.c

// enable C99
// #define __STDC_VERSION__ = 199901L
// $ cc -std=c99 hello.c

// enable C11
// #define __STDC_VERSION__ 201112L
// $ cc -std=c11 hello.c

// C89 main()
// main()
// {
//   printf("hello, C89 world\n");
// }

// C99 and later main()
int main()
{
  printf("hello, C99 and later world\n");
}


// QUESTION: does main need to return an integer ???
// and then there is this - where does this come from?
// int main(int argc, char **argv)
// {
//     puts("hello, world\n");
//     return 0;
// }
