# Pony lang

- no `make` required - `ponyc` will compile and link an executable
- `ponyc` uses the dir name you point it at to name the built executable but
  will build the executable in your cwd
- best bet seems to be to cd into the top level dir of your code and just run
  `ponyc` without args
- it has a GC
- it has traits (ala Java with default impls) and interfaces (ala go)
- numbers can use `_` as separator
    - but only valid variable names can end in primes
- seems to have a strong focus on interfacing well with C

Language

- has both actors and classes
- must have a `Main` actor - it is the entry point
- string literals use double quotes so follow normal C++ rules for that or they
  use `"""` in which case they are "raw data"
- pony has no global variables
- actors can have async methods called `behaviours` - classes cannot
- pony has no null
- types start with capital letter

- private
    - private field only accessible by the same type
    - private constructor/function/behaviour only accessible by same _package_

Reciver capabilities

- `box`
    - the receiver is immutable
    - the default if no explicit receiver capability
- `ref`
    - the receiver is mutable

`=` (assignment) \* is actually the _destructive read_ operation in pony

### Nominal subtyping: Traits

- Pony has _nominal_ ("names") subtyping.
- Nominal subtyping uses the `trait` keyword in pony
- OO inheritance, mixins, traits etc. are examples of nominal subtyping
- you have a class which declares that it has a relationship to some category
  type
    - e.g. Java `class` (concrete thing) declares it has a relationship to an
      `Interface` (a type representing a category)
    - the compiler can then check that the concret thing has access to concrete
      implmentations of all the things that are in the categories that it
      declared itself as being in
- pony traits are bags of method signatures. They can also provide default
  implementations

### Structural subtyping: interfaces

- it's all about how the type is built, not what its name is
- examples from other langs:
    - interfaces in go
- structural subtyping uses the `interface` keyword in pony
- interfaces are named bags of function signatures - they can provide default
  implementations
- a concrete type is a member of the category if it has all the required
  elements - it does **not** ahve to declare itself part of the category

Pony recommends using both interfaces and traits

- interfaces are more flexible so will proably be your go-to
- traits "stop accidental subtyping"
    - what does that mean?

questions

- capabilities secure
- causal message passing
