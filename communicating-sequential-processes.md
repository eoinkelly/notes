# CSP

* a language
* book on CSP: http://www.usingcsp.com/cspbook.pdf
* describes patterns of interaction in a concurrent system
* based on message passing via channels
* is part of a family of mathematical theories called _process algebra_ or _process calculi_

* influenced:
    * occam
    * Go
* the original paper from 1978 by Hoare
    * presented a programming language for concurrent systems
    * was later refined and had the process algebra added by other people
* is a theory for creating modeling how concurrent processes can interact
* there are tools for it that are used in aerospace and other fault-critical industries
* has some similarites with the _Actor model_ in that it is concerned with concurrent processes that exchanage messages. The main differences are
    1. CSP processes are anonymous - AM processes have identities
    1. In CSP the sender can't transmit until receiver is ready i.e. message send and receive happens at hte same time. In AM the send and receive is asynchonous
        * Each style can be used to build the other
    1. CSP uses channels to pass messages, AM sends messages to named actors
* Implementations:
    * Plan 9 Unix had a C implemnetation
    * Haskell has MV ars
    * Go
    * Clojure's core.async
    * Scala has `scala-gopher` lib
    * Ruby
        * https://github.com/igrigorik/agent (not very active)
        * https://github.com/dasch/ruby-csp (abandoned)
    * Rust
        * routines and tasks

Actor model and "CSP model" are the two main choices for message passing programming langauges

Next steps:

* Watch https://www.youtube.com/watch?v=f6kdp27TYZs
* Read the "Seven Concurrency Models in Seven Weeks" book
