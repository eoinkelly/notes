# http://blog.testdouble.com/posts/2015-09-10-how-i-use-test-doubles.html

- Bottom up TDD requires you to come up with your first design on your own
    - ++ it does help you implement that design
    - -- it kinda requires you to have a good design in your head before you
      start
        - this is possibly why it works so well for experienced devs
- Top down TDD
    - tries to help you come up with the design as well as implement it

```
write the first top-level test
? should that be a controller or acceptance test?
? is it a safety test or a discovery test? is it quite integrated?
when you do find an object you need a sense of whether it is collaborator or logic object?
```

@searls objects tend to fall into

1. collaborator objects
    - contain very little logic
    - they gather input for the logic objects
    - they pipeline output from the logic objects
    - use test doubles to discover their dependencies
    - in functional lang they are
    - they don't hold any state about the transaction in progress - the value
      objects do that
2. logic objects
    - tend to be pure functions
    - "logical leaf node"
    - because they are pure functions we don't need test doubles
3. value objects
    - wrap data in the application
    - they are the data that flows through the pipes we created (pipes of
      collaborator and logic objects)
    - provide methods that expose that data
    - these don't use test doubles
4. wrapper objects
    - it wraps a 3rd party API
    - unit tests not that useful - @searls doesn't write unit tests of these
    - he uses the safety net integration tests to catch issues

When he has to make big changes he favours rewrites i.e. don't try to make an
existing small object od something new, instead created a new small object and
change the plumbing

Pros/cons of "Discovery testing"

- intense design pressure yields small focused units
- ++ every method signature and type is validated before it exists - very little
  waste
- refactoring becomes an exceptional case
- -- much complex to learn that than red-green-refactor
- -- radical implementation changes favour resrites over refactor
    - tests are tightly coupled to the implementation
- -- collaborator tests value is front-loaded
    - they tend not to be valued by folks who don't practice this workflow

Most test double libs tend to be better at one style of TDD than another. Older
larger libs tesnd to be unopinionated which makes them confusing for new folks

### VerbNoun.verb naming for objects

@searls creats objects in verbNoun form not NounVerb e.g.

GeneratesSeedWorld vs SeedWorldGenerator BuildsTransaction vs TransactionBuilder

his stated reasons are

1. the verb is what matters
1. over time if the verb stays the same but the noun generalizes you end up with
   slightly cleaner design

he tends to have the main (only?) public method of each of the VerbNoun objects
match the verb

BuildsTransaction.build GeneratesSeedWorld.generate

## How I think about objects

I think I need to get away from objects being nouns/people - they can also be
processes! VerbNoun naming might be a good way to break that habit

## Things to try in my own process

1. try to think about objects in the collaborator, logic, value categories

2. have a box drawing desgin step where I try to break the problem into an
   object tree
    - collaborator (input and output) and logical objects go in the tree
    - have a separarate area for value objects (they get passed around)

    reasons I don't do this at the moment: _ i feel like i own't be able to see
    what objects I need until I have some code _ this might not be true -
    experiment!
