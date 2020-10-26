# Standard ML

## Installation

```bash
# install two SML implementations
$ brew install polyml
$ brew install smlnj
$ brew install mlton
```

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

TL;DR it's a bit of a mess

```bash
##### poly ######
# run interactive env (no readline support :-( )
# `;` seems to be required to tell poly to evaluate the expression, otherwise it just waits for more input
poly


# run the given script (-q hides build output)
poly -q --use hi.sml

polyc -o hello-world ./hello-world.sml # breaks because it can't find main

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

```sml
print "hello\n";

println "hello";
```
