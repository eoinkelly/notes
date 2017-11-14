
#include <stdio.h>
#include <stdlib.h>

static char foo[10]; // declare some bss data
static char *other = "Hello there this is some initialized data";

extern char etext, edata, end; /* The symbols must have some type,
                                   or "gcc -Wall" complains */

int
main(int argc, char *argv[])
{
    printf("First address past:\n");
    printf("    program text (etext)      %10p\n", &etext);
    printf("    initialized data (edata)  %10p\n", &edata);
    printf("    uninitialized data (end)  %10p\n", &end);

   exit(EXIT_SUCCESS);
}
