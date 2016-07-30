# Design patterns

* there are 23 patterns in the book
* the patterns include solutions which have developed or evolved over time
    * => they are not designs people initially tend to generate
    * they are solutions people have evolved into
    * ASIDE: it is no surprise that they often feel like "overkill" given this history
* the patterns represent "common ways that objects can collobarate"
* the goal of the patterns is to make your code more **reusable**
    * => my guess is that design pattern knowledge is most applicable during
      the "refactor" phase where you have a solution and are now trying to find a
      code arrangement to make that solution optimal.
* design patterns are a bunch of solutions with known trade-offs
* the patterns are
    > a record of experience in designing OO software
* they are intended to give you a "leg up" when designing software
* design areas not in the book
    * concurrency
    * any application domain specific patterns e.g. web, game, databases
    * real time programming
    * UI design
    * device drivers
* the patterns in the book are all at a paricular level
    * not as low-level building blocks like linked-lists, hash tables etc.
    * not as high-level as an entire DSL for some application area
    * description of objects and classes designed to solve a general design problem in a particular context
* code examples are in C++ and Smalltalk
    * the patterns are influenced by what can be done **easily** in those languages
        * e.g. if they used C they might have added patterns on "inheritance", "encapsulation",

Pros/cons of the patterns (from my POV)

* ++ if other devs on team are familiar with them then you can communicate architecture very quickly
* ?? if other devs on team are not familiar with them?

The book has 2 parts

1. Chapters 1 & 2: describe what patterns are, how to use them
2. Chapters 3 - 5: the catalog of patterns, divided
    * Purpose (three types)
        * creational
            * deals with how objects are created
            * two subtypes
                * Class
                    * defers some part of object creation to subclasses
                * Object
                    * defers some part of object creation another object
        * structual
            * deals with the composition of classes and objects
            * two subtypes
                * Class
                    * use inheritance to compose classes
                * Object
                    * describe ways to assemble objects
        * behavioral
            * the way objects interact and distribute responsibility
            * two subtypes
                * Class
                    * use inheritance to describe algorithms and flow of control
                * Object
                    * describe how a group of objects can cooperate to perform a task that they could not do individually
    * Scope
        * Class
            * static (fixed at compile time)
        * Object
            * dynamic (can be changed at runtime)

Structure of a pattern

* name
    * allows team to discuss the pattern
* problem
    * description of when to use the pattern
    * sometimes a set of criteria that must be there before you should use it
* solution
    * describes a general arrangement of classes that will solve the problem
    * describes their
        1. relationships
        1. responsibilitie
        1. collaborations
* consequences
    * pros/cons of the solution
    * the trade-offs of the solution


Strategy pattern example

* an object that represents an algorithm
* useful when
    * the algorithm has complex data structures you awnt to hide
    * you want to replace the algorithm either statically or dynamically
    * there are a lot of variants of the algorithm


They make an analogy to a playwrites who often re-use stories that have the same structure.

They mention a number of times that the objects in the pattersn are never found
in the initial stages of design - they emerge when we are trying to make the
design more flexible or reusable.

Design patterns can be considered "techniques for making my existing design
more flexible and reusable". They are not "starting points for my design".


* Each operation defined by an object has a signature
    * signature = operation name, the objects argument it takes as arguments and its return type
* A set of sigatures is an _Interface_.
* A _Type_ is a name used to denote a particular _Interface_.
* An object can have many types
* A type can be implemented by many different objects
* Interfaces can contain other interfaces as subsets
    * type C is a _subtype_ of P if the interface of C fully contains the interface of P. type P is the supertype of C
    * We say that type C "inherits" from type P
* Objects are known only through their interfaces
* An interface says nothing about implementation

## Chapter 2: Case study - Lexi
