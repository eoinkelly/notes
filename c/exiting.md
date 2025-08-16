# Ways to exit a program in C

- processes have a termination status
    - the termination status is an "exit status" if we explicitly exit
- by convention 0 indicates success, non-zero indicates a problem
    - if the process exits to a shell you can inspect the termination status
      with `$?`

#### Exit handlers

There are 3 ways to exit a C program:

1. `void exit(int status)`
    - runs handlers registered with `at_exit(fn)` (in the order registered)
    - does cleanup tasks
        - flushes and closes output streams
        - unlink all files created with `tmpfile`
    - never returns
    - There are some pre-defined exit status constants defined in `stdlib.h`
        ```
        EXIT_SUCCESS // 0
        EXIT_FAILURE // 1
        // examples of usage
        exit(EXIT_SUCCESS);
        exit(EXIT_FAILURE);
        ```
    - `#include <stdlib.h>`
    - `man 3 exit`
    - the value `status` is returned to the parent process and can be collected
      by a `wait()` call (`man wait`)
1. `void _Exit(int status)` or `void _exit(int status)`
    - lower level API
    - I _think_ that `_Exit` is the macro version of the function
    - runs no handlers, exits as quickly as possible
    - it will close open file descriptors but not flush open streams
    - never returns
    - `#include <unistd.h>`
    - `man 3 _exit`
1. `quick_exit()` (requires C11)
    - `#include <stdlib.h>`
    - runs handlers registered with `at_quick_exit(fn_name)` (in the order
      registered) and then calls `Exit()`

### Handler functions

Handler functions have signature `void handler_func_name(void)` i.e. all params
need to come from global variables

Handler function lists:

- `atexit`, `on_exit` register handler functions on the same list
    - `man on_exit`
    - `man atexit`
    - defined in `stdlib.h`
- `at_quick_exit` registers handler functions on a different list (requires C11)
    - no man page for this function ?
    - https://en.cppreference.com/w/c/program/quick_exit

### macOS (BSD) specific stuff

```
# macOS, not linux
man sysexits
man style
```
