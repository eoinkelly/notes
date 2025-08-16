# Preprocessor

- allows you to 1) determine and 2) respond to the features of the various
  environments that your program may compile on

Macro examples

NULL assert isalpha isfinite

type generic math macros

log sin cos pow

There are two kinds of macros

1. expands to an expression
    - we evaluate them, print them or use them in equations
2. block of instructions
    - might appear after an `if` or `while` loop

## #define

- define is a simple text replacement
- the replacement text can be _any_ series of characters
- Note: no semicolon at the end of the line (it is removed before the compiler
  sees the file)
- Assocaiates symbolic names with a constant
- gets substituted in _before_ compilation
- is a simple text replacement
    - Don't use it for types - use `typedef` instead becasue `typedef` knows how
      to handle comma separated variable declarations

```
#define FOO anything
```

## #ifdef

## #ifndef

## #endif
