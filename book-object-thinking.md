# "Object Thinking (Developer Reference)" by David West

* author believes that _behaviour_ is the key concept for
    1. discovering
    1. describing
    1. designing
  objects.
* he likes CRC cards over UML
* he is not into formalism
    * states that "behavioural" approaches to software are indifferent to formalism

* OO design requires a _different way of thinking_ than structural design
* OO design has a different _culture_

OO design has ideas about decomposition that reflect a different worldview/culture from traditional software development
    * worldview: shared socially learned knowledge and patterns of behaviour

He believes that being an agile software developer or an exterme programmer is a cultural thing
    * as such it must be first and foremost lived
    * but the book and can help "sensitize" you to it

Formalism vs Hermeneutics-postmodernism
    * differnt value systems
        * leading to different ideas

Object thinking can be done in _any_ programming language
    metaphor and vocabulary play a large role in object thinking
        * metaphors bridge the familiar and the unfamiliar
        * vocabulary lets us think and communicate

considers models (as in UML) are not the source of truth
    instead treats them as "convenient short-term memory aids"


Behaviour is the cental tenet
    * not class or entity relationships
    * not data attributes and member functions (those being different things)


Other books with the same views

* Object oriented design: brocks, weiner, wilkinson
* Using CRC cards: Wilkinson

UP TO START OF INTRODUCTION

Author believes that XP and agile techniques are entwined with _object thinking_ and each requires the other to be successful.

> “Better people” has been recognized as the most promising silver bullet for
> addressing the software crisis, and yet almost all of our energy has been
> spent on creating better tools, methods, and processes instead of better
> people?”

> “Every effort to advance nonformal, iterative, artistic, and humane ways to
> develop software seem to be resisted and then co-opted and debased by formal
> software engineering?”

Excerpt From: David West. “Object Thinking (Developer Reference).” iBooks.

The author believes that management have been sold the idea that strict formal processes will make better sofware

> The only consistently reliable approach to good software is, simply, good people.

Industry puts so much emphasis on tools and processes because

* not sure how to "upgrade" average developers to good developers
* widespread belief that there are not enough "good" developers to go round
* belief that good developers were "flaky" and not to be trusted - tended to be "artists"

Agile methods focus on attitudes, behaviour, culture and adaptive heuristics
(people things) rather than formal processes

The goal should be producing better developers rather than producing better
software because better software comes from better developers.

Most textbooks on tool and processes mention caveat that you need a group of
experienced developers to make proper use of the tool.

> “ experience is just a code word for those aspects of development—philosophy,
> attitude, practices, mistakes, and even emotions—that cannot be reduced to
> syntactic representation and cookbook formulation in a textbook”

The justifications for XP is that "We have seen master developers do these
things. We have seen less proficient developers do these practices and get
better" rather than any grand theory

> “XP encompasses an oral tradition that has not, as yet, been reduced to ink
> and paper (and maybe cannot be so reduced).”

Author says that most XP books presuppose object thinking - that the two are linked
    Can we unify object thinking with functional approaches?


The ubiquity of OO languages does NOT mean that everyone has mastered OO thinking

> “An argument can be made that the contemporary mainstream understanding of objects is but a pale shadow of the original idea”

Excerpt From: David West. “Object Thinking (Developer Reference).” iBooks.

The behavioural approach to understanding objects is NOT common
    * UML treats objects as if they are "animated data entries" or "mini COBOL programs"

Smalltalk is the programming language that came closest to encapsulating behavioural objects

Stories vs scenarios

* Stories are _always_ told by the customer and depict interactions in the problem domain
* Scenarios (which are part of CRC method) tend to depict things in the solution domain

Author mentions that the 1990's incarnation of CRC cards didn't really use OO thinking

UP TO START CHAP 1

This book puts a lot of emphasis on _thinking_ vs tools or technique

### Comparing UML object and anthropomorphized objects as thinking devices

Structured programming says: a program = data structures + algorithms (+ black box encapsulation)

UML models an object as a "soccer ball" of data surrounded by operations
object thinkers are more likely to anthropomorphize the object

As a "thinking device" the soccerr ball does not focus on behaviour - it treats objects a tiny COBOL programs

anthropomorphize = to project human characteristics onto objects

It seems like the "UML thinking" is closely related to "database design" thinking where data structure is the guiding principle and normalization of data a common, desired task.

### The process we go through when programming

    conception -> decomposition -> analysis -> design -> source code -> object code -> execution
    [discuss things in                                                  [purely the domain of computers]
     problem domain language]

* Object thinking is important at all stages here except for the far left
* Object thinking produces very different objects than traditional structural programming would

The book makes some big claims about OO thinking vs traditional structured programming

* the SLOC metrics will be _an order of magnitude smaller_ using OO thinking!!!
* programs are twice as quick (or quicker) to deliver

Going to need to see something to back that up!

He also has some stats on object size

* responsibilities per class: avg 7
* methods per class: avg 12
* lines of code per method: less than 7 for smalltalk, 15ish for C++, Java

Author says treat these numbers as rough guides, if you have strayed too much then something is wrong


UP TO "THINK LIKE AN OBJECT" P 59

## Thinking like a computer vs thinking like an objec

"Object thinking" means "think like an object" - this is in contrast to "think like a computer" which is what procedural code is.

When we "think like a computer" we write down all the steps a computer would take to solve the problem.

Pros/cons:

* ++ thinking like a computer is intuitive
* ++ it works well for simple problems
* ++ the computers "thoughts" can be quite large steps (that are made up of smaller steps)
* -- if the sequence of external events coming in to the program cannot be controled then the order of "thoughts" can't really be known either
* -- if the problem requires a lot of branching human brains are bad at it
* -- if the computer runs things concurrently (or there is more than one computer involved) then it is basically impossible for us to reason about the order that things will happen in

Many programs are still written in a "think like a computer" style

Eoin: It's not clear whether functional style was considered here when expressing why OO style is better than procedural

Java and UML define an object as data structure + operations - if you think like this then you are not "object thinking" - you are engaging in "computerthink" :-)

> “The essence of thinking like a computer is thinking in terms of the means of
> problem solution. You view every development problem through the lens of the
> means you intend to use to solve that problem. In datacentric development,
> the means of solution consists of relations and relationships; for Prolog and
> LISP, a set of propositions or state declarations.”
>
> Excerpt From: David West. “Object Thinking (Developer Reference).” iBooks.

> “Object thinking involves a very different means of solution—a cooperating community of virtual persons. A virtual person is an entity capable of performing certain tasks.”

* both virtual persons and virtual machines are _metaphors_ for how we think
  about solutions to a problem but they are very different metaphors!

Excerpt From: David West. “Object Thinking (Developer Reference).” iBooks.

> “Object thinking focuses our attention on the problem space rather than the solution space. ”

Excerpt From: David West. “Object Thinking (Developer Reference).” iBooks.

Summary:

* Object thinking focuses on the problem space, computer thinking focuses on the solution space
* Different languages have different tools in the "solution space" e.g.
    * C = procedural tools
    * Lisp = ???
    * Prolog = a set of propositions


Coupling and cohesion

> “In practice problems are not homogenous. They are full of knots and crevices
> which exhibit a well-defined structure. An analytic process fails only if it
> does not take this structure into account.
> —Christopher Alexander, 1964”


* look for "natural joints" (divisions and classification) in the problem space (as a wood carver would look in the block of wood)

> “the naturally occurring modules in a problem space are not isomorphic
> (exhibiting fitness between context and solution) with the modules discovered
> and defined when you are thinking about the design of a computer program.”

His point is that Object thinking looks for the "natural" divisions of responsibility in the problem space and that these divisions are different to those you would come up with if you started from a "I'm going to design a computer program" POV

He asserts thatobject thinking creates objects in the solution space (your program) that are much closer to the natural ones in the problem space and this is a very good thing!


Extreme programming values communication and a _common language_ between customer and developer

> “We will be successful when we have a style that celebrates a consistent set of values that serve both human and commercial needs: communication, simplicity, feedback, and courage.” This statement introduces Kent Beck’s discussion of values in eXtreme Programming eXplained ”

Object thinking facilitates this common language (by having the objects in software match those described by the customer)

Every aspect of object thinking supports the XP goal of communication

* object identification and responsibility assignment are based on the structure of hte problem domain not the structure of some computer program
    * The natural world is complex but humans are good at navigating it - this puts those same skills to use in software
    * Object thinking leads to fewer objects that do less work
    * Focusing on the _coordination of autonomous objects_ instead of the control and management of modules and data structures also contributes to simplicity

Object thinking values fast feedback

* Smalltalk was optimized for this

> “Object thinking embodies precisely the kind of courage advocated by Kent Beck:
>
> Every once in a while someone on the team will have a crazy idea that just
> might slash the complexity of the whole system. If they have courage, they’ll
> try it out. It will work (sometimes).”

