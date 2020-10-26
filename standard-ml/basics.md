# Standard ML

## Overview

* Poly/ML and MLTon seem to be the most popular distributions, followed by SML/NJ
* Folks seem to like Poly/ML for its REPL and fairly fast compilation during dev
* Folks seem to like MLton for generating small fast executables but dislike how slow the compiler is
* SML/NJ has it's own, totally separate, compilation manager
* MLton seems to be a compiler only, Poly/ML also has an interactive environment
* [sourcehut](https://sr.ht/) seems to be popular for hosting repos (as well as Github obvs)
* Standard library is called the _Basis Library_
    * Portable across all SML implementations
    * Spec: https://smlfamily.github.io/Basis/
    * Always available to programs
* Floats are called _Real_

Very good article on building Standard ML:

> There’s little consensus between implementations about how to describe module
> dependencies, pull in third-party libraries, or compile a program that
> consists of more than one file. The standard says nothing about how different
> source files interact. There’s no include or import directive and no link
> between filesystem and module naming. Some implementations do extend the
> standard (most notably SML/NJ adds a few things) but there isn’t any standard
> way for a program to detect what language features are available to it.
> Implementations differ in purpose as well: some SML systems are primarily
> interactive environments, others primarily compilers.
>
> https://thebreakfastpost.com/2015/06/10/standard-ml-and-how-im-compiling-it/

TL;DR compiling larger programs is a bit of a mess

## History

* First compiler 1974
* Popular as a first programming language
* ML for the Working Programmer, 2nd Edition published 1996

* Pre-dates Haskell and is less strict:
    * Has mutable variables and arrays
    * has assignments and I/O commands
* Has a garbage collector
* Designed for theorem proving

## Installation

```bash
# install various SML implementations
$ brew install polyml
$ brew install smlnj
$ brew install mlton
```

## Basic usage

`poly` and `mlton` require different entry-point files - see examples in this dir.

```bash
##### poly ######
# * run interactive env - no readline support :-(
# * `;` seems to be required to tell poly to evaluate the expression, otherwise
# it just waits for more input
poly

# run the given script (-q hides build output)
poly -q --use my-file.sml

polyc -o hello-world ./hello-world.sml

##### mlton ######

mlton hello-world.sml # works generates ./hello-world executable

# -show-basis will create a file with the types you defined in the given script
mlton -show-basis hello-world.basis hello-world.sml
```

## File extensions

* `.sml`
    * Standard ML code
* `.mlb`
    * Specific to MLton compiler
    * let you do multi-file builds by telling the compiler which files to include in the build and where to find them
    * ML Basis file containing a basis declaration
    * ML Basis files are extensions to SML module system to allow for building larger pieces of software
* `.mlp`
    * not sure, might be a poly version of .mlb

## Packages

* no package manager AFAICT
* Poly/ML has a package directory which links to github repos
* `t.sml` file in project root seems to be a convention for tests
* `main.sml` file in project root seems to be a convention entry point

## Language

* Functions cannot apply functions defined after them in the file
* it has exceptions
* `~` is used for negation instead of `!`

* [Standard ML for the Working Programmer](https://www.cl.cam.ac.uk/~lp15/MLbook/pub-details.html) is old but good and free as PDF.
* Great cheatsheet: https://learnxinyminutes.com/docs/standard-ml/


components
    signatures
    structures
        * often a type (all lowercase name) goes together with a structure (Initial cap)
    functors
    struct

Organised by

types
exception identifiers
value identifiers

* SML Basis Library
    * Organises all code into SML modules
    * The top-level environment
        * Includes identifiers for all the modules defined
        * Some _components_ are **also** bound at the top-level so can be used without qualification
            * These are called _pervasive_
        * Also contains overloaded identifiers and infix definitions
            * infix and overloading is only available in the _top-level environment_ AFAICT
    * Some components are required, some are optional. An implementation can choose not to implement an optional component


Primitive Types

1. `array`
    * mutable
1. `Vector`
    * immutable
1. `slices`
    * an abstraction over `array` and `vector` types
1. `option`
1. `list`
    * is primitive
    * defined in top-level env
    * also bound in the `List` structure
1. `char`
1. `string`
    * immutable vector of character
1. `substring`
    * string slice
1. `bool`
    * is primitive
    * defined in top-level env
    * also bound in the `Bool` structure
1. ``
1. ``


```sml
(* define a new module *)
structure Foo =
struct
    datatype
    fun
    local
    in
end
```

sturcture --> value
signature --> type
functor --> function

* Structures
    * A package of related types, values, functions
    * They do not hide information - they just let you put related things in a namespace
    * They do not create a new type!
* Signature
    * lets us specify what components a structure **must** contain
    * is essentially the type signature of the structure

Functor

* A function is an ML expression that takes parameters
* Applying a function substitutes the argument values for the parameters
* A function can only be applied to arguments of the correct type

* A _functor_ is a thing which takes other structures as parameters
* Applying a functor substitutes the argument structures for the paramenters
* A functor can only be applied to arguments which match the correct **signature**

* Introduced with the `functor` keyword

Functors are a kind of structure factory

The definition of

Functor creates structure customised to a particular concrete type that must satisfy the signature given to the Functor in its declaration

Functor F declares that it takes as parameter a structure satisfying signature S. It's declaration contains values etc. whose types reference signature S.
Functor F when given a structure A which satisfies signature S will return a structure whose types are customised to A


Functor A, given signature S will return a struct containing functions which operate on the
* ??? a function that can take different types (or has multiple heads)


