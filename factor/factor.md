# Sources

* https://www.youtube.com/watch?v=f_0QlhYlS8g
* Seven more langs in seven weeks book

> Factor belongs to the family of concatenative languages: this means that, at
> the lowest level, a Factor program is a series of words (functions) that
> manipulate a stack of references to dynamically-typed values.

* Functions are called _words_.
* Things on the stack are dynamically typed.
* Has a garbage collector
* Compiled to native code (but also has REPL)
* Whitespace is used to separate words (factor doesn't care what kind of
  whitespace you use .e.g.
* Uses postfix notation (so called reverse polish notation) so has not operator
  precedence
The stack is used to organise calls to words, not as a data structure

* Includes a GUI toolkit based on OpenGL
* Has a HTTP server, web framework and DB adapters
* Uses an image a bit like smalltalk - compiled code and data are stored into
* the image and then the image is saved
    * Has a special tool for building a minimal image when deploying a
    * stand-alone application
* Is typed
* Has an FFI for interfacing with C etc.

> Expressing yourself with pure composition and without locals or named
> arguments is the key. It's extremely succinct with no syntactic overhead.
> Composition makes it very easy to factor out redundancy and to "algebraically"
> manipulate your code; boiling it down to its essence.

```factor
[ * ]
```

is considered the same as

```
[
*
]
```

## Listener UI

```
Ctrl+p = prev line of history
Ctrl+n = next line of history
Tab = show tab completions
```

## words

Some commonly used words

```
clear   = clear the stack
.       = pretty print the thing at the top of the stack
length  = replace any "sequence" at top of stack with its length
```

* comments begin with `! ` (the space is important)

### Stack effect

* An annotation for functions
* A description of what the word (function) does to the stack

```
( things-off -- things-on )
(x y -- z) = 2 things off, puts one thing on
the things can be named anything to help document the word
```

### Vocabularies

* Words are organised into _Vocabularies_ (as functions are organised into
  modules in other langs)
