# Ways to exit a program in C

* processes have a termination status
    * the termination status is an "exit status" if we explicitly exit
* by convention 0 indicates success, non-zero indicates a problem
    * if the process exits to a shell you can inspect the termination status with `$?`

```
// man(2)_exit
_exit(int status)
// aliased to ...
_Exit(int status)
// these functions do not call any handlers registered by `atexit` and `at_exit`
// open stdio streams are not flushed
// it will close open file descriptors

// man (3) exit
// this is a higher level API and is preferred
exit(int status)
```

* `exit`
    * terminates the process immediately
    * does not return anything
    * any open file descriptors are closed
    * the value `status` is returned to the parent process and can be collected by a `wait()` call (`man wait`)

* exit handler functions
    * they run functions registered to run at process exit
    * handlers
        * `on_exit`
        * `atexit`

### Exit status

There are some pre-defined exit status constants defined in `stdlib.h`

```
EXIT_SUCCESS // 0
EXIT_FAILURE // 1

// examples of usage
exit(EXIT_SUCCESS);
exit(EXIT_FAILURE);
```
