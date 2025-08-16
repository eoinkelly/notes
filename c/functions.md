## functions

- can be available
    - globally
    - just in the current file
- function arguments can be evaluated in _any_ orer (the compiler is free to
  choose)
- all function arguments are passed by value in C - everything is copied!
    - you can "modify" an argument if it is a pointer and you mutate what it
      points at but the pointer is still copied
    - the name of an array in C is a pointer to its first value so the whole
      array is not copied, instead the pointer to the first value is
- `int` is the default return type of functions in C - if you do not specify a
  return type the `int` is assumed (but compiler will probably warn you)

consider

```c
int do_thing(int a, int b) { return a + b; }
int x = 3, y = 4;
do_thing(x, y);
```

when `do_thing` is called first _copies_ of `x` and `y` are created and assigned
as local variables `a` and `b` in `do_thing`

```
return-type function-name(param1-type param1-name, ...) {
    variable-declarations
    statements
}

parameter == formal argument == the variable named in the parameterized list of a function definition
arugment == actual argument == the value actually passed to the function when invoked
```
