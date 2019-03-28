
Online converter (also available as cmdline tool): https://cdecl.org/

Tutorials

* http://unixwiz.net/techtips/reading-cdecl.html
* http://ieng9.ucsd.edu/~cs30x/rt_lt.rule.html


A C type declaration has

1. Exactly one basic type
2. 0 to many derived types

C has three kinds of Derived types

| Symbol | reads as ...                       |
|--------|------------------------------------|
| *      | ... is a pointer to ...            |
| []     | ... is an array of ...             |
| [n]    | ... is an array of n ...           |
| ()     | ... is a function returning ...    |
| *const | ... is an immutable pointer to ... |


If the basic type is prefixed by `const` then read it as _an immutable TYPE_.

```c
const int x1; // x1 is an immutable int

int *const a1; // a1 is an immutable pointer to a (mutable) int
int *const a2 // a2 is an immutable pointer to an immutable int
```

Note the "to", "of", "returning" suffixes are really important in how you read types

* derived types are "modifiers" - they modify something which **follows** not the thing which came before them as you read it

Reading order

1. Start with the variable name
2. Always end with the basic type
3. Go right when you can, go left when you must

The only thing which stops you going right is a `)` which is part of a
grouping, not a "function returning"

You can tell the difference between `)` being part of a group vs a "function
returning" because function returning is either `()` or `(type1, type2)`


You sometimes see type declarations without a variable name in

1. sizeof
2. casts

In this case you have to start by mentally finding where the variable name
would go, adding it and then proceed as before.
