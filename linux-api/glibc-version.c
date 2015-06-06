#include <stdio.h>
#include <gnu/libc-version.h>

int main(int argc, const char * argv[]) {
    printf("Gnu LibC Version: %s\n", gnu_get_libc_version());
    return 0;
}
