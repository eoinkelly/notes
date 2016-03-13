
## IO in C

* C has a simple model of IO: text input and output is always represented as streams not matter where it is coming from or going to
* a stream is a collection of lines
* each line is 0+ characters terminated by a newline


```c
c = getchar() // reads the next character (byte) from an input stream
putchar(c) // writes a single character (byte) to an output stream
```

* getchar()
    * expands to `getc(stdin)`
    * returns an "unsigned char" converted to an int
    * returns a special `EOF` value when it reaches the end of the stream or an error occurs
        * EOF is a "negative integral constant"
        * EOF is an integer defined in stdio.h
        * the specific numeric value does not matter as
          long as it is not the same as any possible
          character

## `scanf`

* Reads a string from STDIN, optionally parses bits out of it  and stores stuff
  in a memory location.
* First arg is a format specifier.
* Second arg is the _memory address_ that the data should be put.

```
scanf( "%s", &ages[3] );
```
