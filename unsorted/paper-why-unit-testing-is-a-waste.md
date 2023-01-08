# Why most unit testing is a waste

* by James Coplien
* http://www.rbcs-us.com/documents/Why-Most-Unit-Testing-is-Waste.pdf


In traditional computing
    * computational units = functions + procedures
    * the "design unit" was "things that computed"

* JC says you could reason about the code in a code review back then

design unit = function/procedure

* OO turns this upside-down
    * The "design unit" is a "small hetreogenous composite"
        * it combines several programming artefacts (functions & data) into one wrapper
        * it combines a bunch of functions with all their state global to the functions
        * it makes "mini globals"
    * The class became a "factory" or "cookie cutter" that objects were made from at runtime.
    * The exact function that will be called is determined at run-time so you can't reason about it.
    * It became impossible to reason about the run-time behaviour of code by inspection alone.
    * He contends that
        * this inability to reason about the code by inspection was why testing became popular again
        * design became more data focused because objects were shaped more by their object structure than by the properties of their methos
    * classes became the unit of
        1. code analysis
        2. design (antropomorphic design)

* JC laments that you cannot reason about code by inspection
* He points out that it is pretty much impossible to test all the possible states an object can be in
  * even a trivial object can have trillions of possible states

* he describes polymorphic function call as a "hypergalactic GOTO"

* he says if you are changing you architecture to make your tests easier you are doing it wrong
* by "easier" he refers to "code coverage"

The founders of the "lean/scrum" in Toyota were very much against the automation of intellectual tasks

He advocates doing testing at the system level - makes analogy to how hardware tests w. pin-outs etc.

He is fine with system testing, just doesn't like unit testing

He says avoid automating things that a human needs to do - like designing the architecture of a system and futher that
focusing on automating this kills the "creativity *required* to do it"

"Testing does not increase quality, programming and design do"
"*Good* testing is based on *risk management, which is in turn based in information theory"

From the information theory POV
  a test that always passes (where pass = 1, fail = 0) that is run 32 times has 32 bits of data but 0 bits (maybe 1 bit) of information
  * the "information"/"usefulness" of a test is based on how likely it is to pass.
  * conversely if a test never passes it has very little information either

He says:
  * to reduce test mass, look at tests have have either not failed, or not passed for 1 year (or some other period of time)

He also calls out "tautalogical" tests as being useless and a frequent by-product of TDD

ME: There is a case for not leaving all the tests you made TDDing in the final code base

He contends that the only tests that have any value are "business tests" - "most unit tests are deived from programmers fantasys about how it should work"

"If this test fails, what business requirement is compromised?" if you can't answer that then what is the benefit of the test (because it definitely has a cost)

ME: a focus of this article seems to be shops that have a large amount of tests - perhaps too many and they want to get rid of some

He calls out the agile manifest as too focused on "doing" rather than "thinking"

He thinks that having assertions in your code that test assumptions and having the build tool remove them is a great idea
* he thinks you could leave the assertions and have them report the failure in production
* he believes that much unit testing is just these "assertions" in disguise
