# Clojure

- runs on the JVM
- `,` is treated as whitespace so can be added if you like
- is opinionated - does not try to have every feature
- almost any element of it can be redefined at runtime
- is a good citizen of the JVM
    - clojure code compiles to the same bytecode as ordinary Java code
    - clojure string _is_ a java string
    - clojure function call _is_ a java method call
- features of clojure:
    - Software transactional memory library (STM)
        - allows you to share memory between threads without locking
    - functional programming
        - uses pure functions ?? a lot/ always??
        - first class functions: can be stored, passed around and returned just
          like any other value
    - agents
    - arbitrary polymorphism
        - polymorphism: the ability of a function or method to have a different
          defn. depending on the object it is called on.
        - clojure has 2 ways of doing polymorphism
            1. multimethods
            2. protocols
            ````clj
            ; Thingable interface has 1 method: dothing
            ; dothing takes two args and concatenates them together
            ; extend String type to implement the Thingable interface
            (defprotocol Thingable (dothing [a b]))
            (extend-type String Thingable (dothing [a b] (.concat a b)))
            ; notice that we basically monkey-patched String here
                ```
            ````
    - clear distinction between identity and value types
- takes idioms from lisp and java
- immutable local variables
- "peristent" data structures
- in ruby "class" acts as a namesapce for data _and_ methods. In clojure there
  are separte "function namespaces" and "type namespaces"
- in ruby method implementations are embedded throughout the heiarchy chain. In
  clojure "interface definitions" are separate from "function definitions"
- mathematical objects are defined by sets of values and transformed via
  functions. FB is much closer to this than OO. Does this convey some advantage
  to FP?
    - ?? maybe more reuse of mathematical concepts ??
    - mathematicans choose to reason this way presumably because it is simpler
      than reasoning about objects?

### Aside: lisp

The original paper described all of computation in terms of just 7 functions and
2 special forms:

1. atom
2. car
3. cdr
4. cond
5. cons
6. eq
7. quote

8. lambda
9. label

- clojure has
    - `first` for `car`
    - `rest` for `cdr`
    - `cons`

### Aside: iSeq interface

- an interface implemented by list, vector, map, set
- functions such as `map` work with anything that implements iSeq

### Aside: special form

A special form is a primitive that is evaluated using different rules than
standard evaluation

clojure special forms:

- fn
- defn
- ???

Clojure also has macros

- declare

### Evaluation order

clojure evaluates nested lists from the inside out

```clj
(foo (bar1 a b) (bar2 a b))
```

The evaluation order is

1. bar1
2. bar2
3. foo

so clojure does the args first

### Aside: on state, value, identity

Why "mutable state" is an oxymoron:

The "identity" of an entity is the combination of the values of its properties
now and at all times in history.

At a single instance in time the values of the properties are the entity's
"state":

- => state is a thing defined at a single instance of time
- => the entity can move from one state to another but a state itself cannot
  change _by definition!_
- => there is no "mutable state". The state at time A and the state at time B
  are just different states - there is no sense that A turned into B

"concresence": the growing together of parts original separate

In OO programming there is no separation of "state" and "identity"

- OO objects live in an endless now
- In OO we work with things that are "the concresence of states" not a
  particular state
- I have a reference to an object in memory but I don't know what _state_ I have
  a reference to - it may change over time.
- In FP I get a reference to a _state_ in memory

## Leiningen

- "gem + bundle + rake" for clojure
- clojure is just a java lib so you need extra stuff to make the ecosystem (and
  plugin to the existing java ecosystem)

```sh
# launch clojure repl manually
$ java -cp clojure-1.6.0.jar clojure.main

# use lein (which is a better repl btw)
$ lein repl


lein new compojure-app guestbook
# seems to automatically fetch the clojars it needs

cd guestbook
lein ring server
# seems to automatically fetch the clojars it needs

```

```clojure
; repl basics

(+ 1 2 3)
(println "Hello world")

; show an alert box
(javax.swing.JOptionPane/showMessageDialog nil "Hi")

; Exit the repl
(System/exit 0)
```

# Encapsulation

? methods

- clojure does not do data hiding encapsulation

1. function namespaces

    ```clj
    ; create a new namespace
    (ns eoin.chess)

    ; create new named piece of data
    (def board [1 3 4 6 9])

    ; create a function
    (defn doThing [] ( ... ))

    ; create a namespace private function
    ; only accessible from things within the same namespace
    (defn- foo
    ```

2. lexical closures
    ```clj
    (let
    (letfn
    ```
    They let you create "local varialbe areas" at the top of your function. This
    seems to be a pattern in FP - an area where you setup all the local vars you
    need for a computation
    - ++ keeps functions short as setting up lots of local state seems ugly
    - the lifetime of the local state is clear but (I think) no more so than in
      OO languages

## Creating variables

- the `def` special form creates a identifer for some data/code in the ??? scope

```clj
(def pi [3 1 4 5 9])
```

??? the symbol is available directly in the current scope but also has a named
scope ???

    pi
    myproj.core/pi

## Scalar data types

- Vars
    - mutable storage locations
    - can be bound and re-bound on a per-thead basis. ???
- Symbols
    - used as identifiers for variables
    - can have +-=?! and alphanumerics but no spaces
- Keywords
    - symbols that refer to themselves e.g. `:foo`
    - ruby calls symbol what clojure calls keyword
    - are "self evaluating" i.e. they evaluate to themselves
- Number
    - are "self evaluating" i.e. they evaluate to themselves
    - will expand storage required for the number automatically
    - uses Java number types underneath the hood
    - radix notation up to base 38 e.g.
        - `2r001101101` binary
        - `16r0A456F` hex
        - `0x0A456F` hex
        - `0177` octal (WARNING: easy to fuck this up)
    - has a rational number type`22/3` (is automatically simplified where
      possible)
- Boolean
    - `true`, `false`, `nil`
- String
    - are "self evaluating" i.e. they evaluate to themselves
- Character
    - denoted by `/a`
- Regular expressions
    - prefix by hash symbol, example??

Clojure data types can have a Map of metadata associated with them that
describes them

- metadata is ignored in equality tests

### Collection data types

- List
    - Construct a list data structure: `\`(1 3 5)`or`(list 1 3 5)`
    - clojure expects the first element of a list to be callable unless you
      quote it
    - sequential lookup time
- Vector
    - `["a" "c" "f"]`
    - `["a", "c", "f"]`
    - logarithmic (near constant) lookup time
- Map
    - `{:foo "bar" :blah "bleh"}`
- Set
    - `#{"a" "b" "c"}`

Note that clojure _code_ is represented using these collection types too e.g.

- a variable definition is a list
    1. a spexcial form `def`
    2. a symbol (the identifer name that the data will be bound to)
    3. some other clojure data structure
- a function definition is a list containing
    1. a spexcial form `fn`
    2. a symbol (the identifer name that the function will be bound to)
    3. a vector (the arguments)
    4. 0+ lists that are the statements in the function

In ruby this would be something like

```ruby
[:def, :foo, [:a, :b], [:puts, :a], [:puts, :b]]
```

## Functions

a function call is a list containing 1. function name 2. optional string
documenting the function 3. optional map of some metadata about it 4. vector of
parameters (required) 5. 0+ lists that are the calls to perform the function

the result of the last top-level list within the function list is the return
value.

- clojure will throw an `ArityException` if you pass the wrong number of args to
  a function

Anonymous functions are created via the `fn` special form or via the `#()`
syntax sugar

```clj
; these are equivalent
(fn [arg] (println arg))
#(println %)
#(println %1)
```

- `%` = `%1` = shorthand for unnamed first positional arg
- `%2`, `%3` etc. are the other positional args

Named Function

- Named functions are just anonymous functions bound to a symbol used as an
  identifier
- the `defn` special form is syntax sugar for this

```clj
;; these are equivalent
(def foo (fn [a] (println a)))
(defn foo [a] (println a))
```

clojure uses a single pass compiler so functions must be declared using the
`declare` _macro_ before they are used

```clj
(declare foo)

(defn bar [] (foo 1 3))

(defn foo [a b] (println "hi"))
```

### Chaining functions

Normally you would next lists to chain functions

QUESTION: what does (apply ...) do?

Clojure can do closures just like JS i.e. you can have a factory function that
returns a function and closes over the params you gave the factory

Local variables

```clj
(let [aa 12 bb 14] (println aa) (println bb))
```

- use `let` to make "contexts" for local variables
- `let` takes as args
    1. a vector of symbol and value pairs. Each value is bound to the
       corresponding symbol
    2. 0+ lists to evaluate. The names form the name-value pairs are available
       within the lists
- Why doesn't let use a map rather than a vector?

### Threading

To avoid overly nested expressions you can use one of the two threading macros

1. `->>`
    - thread operations from left to right, the result being passed as the
      _last_ arg to the next expression
2. `->`
    - thread operations from left to right, the result being passed as the
      _first_ arg to the next expression

```clj
;; equivalent lines
(reduce + (interpose 5 (map inc (range 10))))
(->> (range 10) (map inc) (interpose 5) (reduce +))

;; clojure will evaluate these lazily

;; both of these do the same amount of work because of lazy evaluation
(->> (range 9) (map inc) (interpose 5) (take 5))
(->> (range 99999999999999999999999) (map inc) (interpose 5) (take 5))
```

### Destructuring

(let [a b] (some-form-that-returns-vector-of-two-things) (do-stuff))

;; foo expects a vector of 3 args, the first of which is a vector of two, that
;; we immediately destructure into the bits we need (defn foo [[a b] b c] (stuff
...))

#### variable length args

- You can handle variable length args using `(defn foo [& myargs] (...))`
- the arguments provided to foo will be stored in the "sequence" myargs

TODO: there is more to this & stuff (Sun 10 May 12:44:04 2015)

There is some syntax sugar for destructuring keys out of maps

```clj

(defn foo [{:keys [id pass pass1] :as user]
    ;; the symbols: id, pass, pass1, user will all be available in the function
    (...))
```

### Namespaces

- _functions_ in clojure are in namespaces

```clj
;; declare namespace
(ns foospace)
```

There are 2 ways to refernce a function from another namespace - we can pass
more args to `ns` to import other things

1. `:use` keyword
    ```clj
    (ns foo (:use otherspace))
    ```

    - all "Vars" in otherspace become available in foo
    - -- if namespaces register the same function name and we import both we
      will get an error
    ```clj
    ;; only import the named functions from otherspace
    (ns foo (:use [otherspace :only [func1 func2]]))
    ```
2. `:require` keyword

    ```clj
    (ns foo (:require bar))
    ```

    - All "Vars" from bar will be imported into foo as `bar/\*` i.e. they are
      namespaced
    - ++ more explicit about the origin of the vars
    - we can alias namespaces as we import them:
      `(ns foo (:require [somelongnamespace :as lng]))`
    - require can do the same thing :use does by using the :refer keyword:

    ```clj
    (ns foo (:require [bar :refer :all]))
    ;; equivalent to
    (ns foo (:use bar))

    (ns foo (:require [bar :refer [func1 func2]))
    ;; equivalent to
    (ns foo (:use [bar :only [func1 func2]))
    ```

QUESTION: what are "Vars" QUESTION: can you have multiple namespaces per file

### Dynamic variables

- clojure supports "dynamic variables" which function much like variables do in
  OO languages.
- They are not encouraged but are useful for file descriptors etc. e.g. things
  we get a reference to that don't map well to our value based world
- notice that you don't get to change the value whenever you want e.g. it stays
  the same in a particular context delmited by `binding` form

```clj
;; create a dynamic variable (one whose value can be changed) i.e. a variable
;; that works like other languages
(declare ^{:dynamic true} *foo*)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]

  ;; invoke functions from the other namespace
  (ek/hithere)
  (ek/byebye)

  ;; bind the dynamic variable to a value and then use it
  (binding [*foo* "I am dynamice foo"] (println *foo*))

```

### Java interop

```clj
;; :import is how we pull in java classes
(ns myapp (:import java.io.File))
;; shorthand to pull multiple classes form same package
(ns myapp (:import [java.io File FileInputStream FileOutputStream))

;; create instance of File
(new File ".") ;; using `new` form
(File. ".") ;; shorthand

;; create instance of File
(def f (File. "/etc/things"))

;; call method on the instance
;; preceeding `.` makes them visually different from other clojure methods
;; Format is: (.methodName objectInstance)
(.getAbsolutePath f)


;; call static method
(:import Math)
(Math/sqrt 16) ;; => 4

;; in the repl
(:import java.io.File)
(def ff (java.io.File. "."))
(.getAbsolutePath ff)

```
