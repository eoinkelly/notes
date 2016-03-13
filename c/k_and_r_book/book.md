
## Chapter 5: Pointers & Arrays

* "pointer to OTHER_TYPE" is a data type in C
* there is no such thing as "a pointer" - it is always "a pointer to X"
    * every pointer points to a specific data type
* pointers take up a fixed amount of storage which varies by system (8 bytes on my laptop)

```c
int *x; // reads two ways:
// "x" is going to be a pointer to an int
// "*x" is going to be an int
```

### & operator

* only applies to objects in memory (variables and arrays) - it cannot be used with
    * constants
    * expressions
    * register variables

* can be used to get a pointer for an existing data type
```c
int x = 12; // create automatic variable x
int *xp = &x; // create a pointer to x
```
* C passes all arguments to functions by value but pointers allow you to work around this
    * it is clear in the syntax when you are manipulating a pointer vs manipulating a value directly
        * => so pointers are not the same as "pass by reference" where that difference is not visible
* pointers can be used to approximate "mulitple return values"
    * you can pass "in/out" pointer that the funciton can fill and it can use
      its return value to signal something else e.g. failure, end of file etc.

### Generic pointer

* is the exception to the "every pointer points to a single specific type" rule
* cannot be dereferenced itself ???
* In _very_ old-school C `char *` was the pointer type for a generic pointer
* In everything since `void *` is the correct type for a generic
