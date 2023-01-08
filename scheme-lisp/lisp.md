# Lisp

* Sources
    * Land of Lisp 2010
* Lisp is a family of languages
* Features
  * minimalist design, simplest possible language syntax
  * popularity lived and died with AI as a research area in the '70's and '80's
  * pioneered ideas like GC and parametric parameterization
  * based on work by John McCarthy in 1959
        * Recursive Functions of Symbolic Expressions and Their Computation by Machine, Part I
        * http://www-formal.stanford.edu/jmc/recursive.pdf
  * easy code as data, easy metaprogramming
  * standardised by ANSI in _ANSI Common Lisp_ but Scheme is also popular
  * _ANSI Common Lisp_ vs _Scheme_
        * _ANSI Common Lisp_
            * built the original lisp dialects so has design warts from them
        * _Scheme_
            * Originally called schemer
            * Scheme standards
              * R6RS (2003)
              * > The R6RS standard has caused controversy because some see it
                > as a departure from the minimalist philosophy. In August
                > 2009, the Scheme Steering Committee, which oversees the
                > standardization process, announced its intention to recommend
                > splitting Scheme into two languages: a large modern programming
                > language for programmers; and a small version, a subset of the
                > large version retaining the minimalism praised by educators and
                > casual implementors.
            * Aims to be a more pure implementations of the lisp ideas than Common Lisp
            * tends to be a bit more verbose
            * In many ways, Haskell is an evolution of the ideas in Scheme
        * Many newer lisps try to combine the best of Common Lisp and Scheme
    * Comparison: https://ecraven.github.io/r7rs-benchmarks/
* Lisp dialects (any language which obeys the central principles of lisp is a dialect)
    * Clojure
        * https://clojure.org/
    * Common lisp
        * Has many implementations https://common-lisp.net/implementations
        * https://clisp.sourceforge.io/
        * supports object oriented programming
    * Racket
      * https://racket-lang.org/
    * Chicken scheme (a scheme)
        ```
        $ brew install chicken
        $ csi
        ```
    * Lisps designed to script existing larger applications (not typically used for creating standalone applicaations)
        * Gnu Guile (a scheme)
            * https://www.gnu.org/software/guile/
        * Elisp (emacs lisp)
        * Script-fu Scheme
            * Used in GIMP editor
    * Arc
        * By Paul Graham
        * Implemented in Racket
        * Doesn't look very active or maintained
    * Chez scheme
        * https://www.scheme.com/
    * ... Various others ...
* Lisp file extensions
  * `.scm` (scheme)
  * `.lisp` (lisp)
  * `.lsp` (lisp)
  * `.cl` (lisp)

```
$ clisp
  i i i i i i i       ooooo    o        ooooooo   ooooo   ooooo
  I I I I I I I      8     8   8           8     8     o  8    8
  I  \ `+' /  I      8         8           8     8        8    8
   \  `-+-'  /       8         8           8      ooooo   8oooo
    `-__|__-'        8         8           8           8  8
        |            8     o   8           8     o     8  8
  ------+------       ooooo    8oooooo  ooo8ooo   ooooo   8

Welcome to GNU CLISP 2.49.92 (2018-02-18) <http://clisp.org/>

Copyright (c) Bruno Haible, Michael Stoll 1992-1993
Copyright (c) Bruno Haible, Marcus Daniels 1994-1997
Copyright (c) Bruno Haible, Pierpaolo Bernardi, Sam Steingold 1998
Copyright (c) Bruno Haible, Sam Steingold 1999-2000
Copyright (c) Sam Steingold, Bruno Haible 2001-2018

Type :h and hit Enter for context help.

[1]> (+ 3 (* 2 5))
13
[2]> (quit)
Bye.
```