# Reviewing tests in code review cheatsheet

# rough notes

* the world is expressed through the messages we send to other objects and self

* there is a very large number of invalid inputs for most messages e.g. if it takes string or int
* there is an infinite number of invalid inputs (except if compiler enforces some strict type check)
* there are (close to) infinite no. of valid worlds for most messages
* there are (close to) infinite no. of invalid worlds for most messages

* what is a valid world?
    * where all the replies to each of the outgoing messages are in the set of
      replys that our message can return a correct result or trigger the correct side effects
* if the subject sends any outgoing or internal messages at all then the no. of
  possible worlds it can be in is infinite - e.g. a string or integer return
  value can have infinite (in theory anyway) values

* => I cannot test all possible invalid worlds and inputs.
* => all my tests will by necessity be "representitive examples"
* => my tests should try to represent a whole category of worlds+inputs e.g.
    * text xyz checks what happens if the input string is too long
        * this represents all the worlds where the input string is too long (from 1 byte too long to infinity bytes too long)

Tests are "representatives of their category of invalid input or invalid world"!!

Ideas:

* Categorise inputs
    * those from user at runtime - more likely to be error filled
    * those from developer at "compile" time - is a much smaller set of possible inputs
* explicitly do checks to reduce the size of possible valid inputs
    * e.g. strings must be lower case a-z only and no longer than 1k

* QUESTION: is thinking about possible paths through my code useful?
    * coverage says you should have a test that exercises each path through a message
    * that is not the same as exercising all possible paths through a message

* QUESTION: is making the set of valid inputs smaller by explicit parameter checks useful?


* IDEA: layers of tests
    1. a single test that represents an example of a valid world and valid inputs
        * maybe one "valid inputs, valid world" test is enough?
        * what is the value in testing many valid inputs?
            * its not really documenting much for the next dev


# Unit tests

## Conclusion: steps to follow in reviews

1. pull the branch and run the tests for that file
1. read the doc output
    * do I get a sense of the purpose of this class?
    * can I see what the public API is from this output?
1. reads the it block of the one they are interested in
1. for each message
    * is it command, query, both?
        * if it has a query component: is return value being asserted?
        * if it has command component: are the public, direct, side-effects
          being asserted?
    * identify what invalid inputs and invalid worlds are for this message
      response
    * are there tests for
        * valid inputs + valid worlds (happy path)
        * invalid inputs + valid worlds
        * invalid inputs + invalid worlds
    * are there expectations on all outgoing command messages
    * is the locality of test code good?
        * i think that tests should be self contained within each message
          describe
        block (not the it blocks) but that the it block should explicitly build
        all the things it uses e.g. no magic introduction of variables
    * are the given, when, then stanzas clear within each test

## *every* public method should have a describe block (not 100% sure about this)

* even simple ones should be visible in the test output for the reader
* what about super simple ones that don't need tests?
    * ++ makes the tests be better docs
    * -- not very high-value tests for checking correctness of the module
    * ++ does make you think about whether that method really should be public

## what jobs do unit tests have?

1. verify and demonstrate the class responds appropriately to the message with
   valid and invalid parameters
1. verify and demonstrate the class responds appropriately to the message with
   the world setup in different ways (edge cases)
1. side effect: provide a way to easily re-run those verifications in future to
   prevent regressions
1. side effect: demonstrate what valid input for this message looks like
1. side effect: demonstrate what valid world setup for this message looks like

Thinking hints:

* think in messages as we are sighting along the edge of the capsule!
* the "response to a message" is the atom of unit testing.
    * A class has no behaviour until it receives a message
    * A class can only respond to one message at a time
* it is good to separate errors into "invalid input" and "invalid world setup"

### strategies for top level of grouping tests in a file

1. by receivable message (aka public methods)
    * ++ is a fairly logical way to group
    * behaviours that are common to many message responses in the class can be
      put in shared_examples.
        * ++ allows reader to read just the "describe block for this message"
    * can have sub contexts for
        * valid input, valid world
        * invalid input, valid world
        * invalid input, invalid world
1. by valid input, invalid input, valid world, invalid world etc.
    * e.g. all tests to do with using this object as a particular kind of user
    * e.g. all tests where the "world" is setup in a particular way
    * -- doesn't make it as easy to see the structure of the class in the test
      output
    * ++ if "world setup" is slow to run e.g. database setup and teardown then
      you might have to do it this way

aside: if world setup is painful it exerts a strong force on doing it that way

aside: if execution time is painful it exerts a string force on the tests -
makes you want to do more assertions per run

### Groupings of tests in a file

```
ClassName
    describe: class messages

    describe: instance messages
        describe: message name
            context: valid inputs   + valid worlds
                tests
            context: invalid inputs + valid worlds
                tests
            context: invalid inputs + invalid worlds
                tests
```

## Specific RSpec features

In general I think I am favoring using the minimum amount of rspec sugar - it's
just more stuff that a reader has to understand to grok the tests.

### subject

* -- I think it throws away an opportuntiy to use a better name for the subject
  e.g. `foo_with_some_val`
* ++ It is quite a descriptive name I guess as long as it is clear exactly what the subject is
* -- just some sugar syntax that doesn't add much value - `let(:subject) { ... }` is clearer

### On let and let!

* be careful not to use them to make specs too dry
* ++ let is very convenient
* -- let is most convenient if you are creating slow collaborators that you want to memoize
    * might be much less of an issue if test style is mockist ???
* -- let! is less useful, might be better in a before block ???

### rspec custom matchers

* ++ they do read nicely and make the test less noisy
* -- they hide the detail of how the test passed/failed
* -- they hide an opportunity to show to to introspect the object
* -- not likely the reader will know exactly how they work without searching

### shared examples

* ++ good for behaviours that are common to 2+ methods

# naming of doubles

* I now prefer naming doubles without a prefix e.g. `foo` not `fake_foo` or `foo_double`
    * the suffix seems noisy and unnecessary - it shouldn't matter in the
      context of the test whether the collaborator is real or not

# On DRY in tests

* if code is not in the it block in question where is it acceptable for it to
  be
    * in same file
        * before block
    * in other file
        * as shared module
        * as custom rspec matcher

what are pros/cons of moving functionality out of the it block???

* I now believe that tests should not be dried up outside the boundary of the
  method level describe block. Reasons:
    * test readers will start at the it block but it is probably overkill to
      repeat *everything* in each it block
    * moving code outside of this is tempting but makes the tests less "local"
      so should happen rarely

## aside: sandi metz rules

Clarifying question: for each message, is this message a command, query or
both?
    where are the query tests that assert ret val of the message
    where are te command tests taht assert state of world after message send


* when testing the outgoing messages that result from an incoming message I
  should test them even if they originate from a private method that was
  triggered by the public method
    * that detail is just an implementation detail - from outside we can
      comceptualise of the method being completedly inline
* should test _all possible outgoing messages_

What are the actions my method can take on the system as a whole

1. do nothing to the state of the system (query method)
2. send command messages to collaborators
    * included in those messages are the exceptions it can raise
    * included in these messages is "changing the state of collaborators"
      (which we can only do via messages)
3. send query messages to collaborators - ignore.

### What are exceptions (hint: return values)

* An exception is the argument to the special "there was an exception" message
  that is sent to each method in the current callstack in turn.
* So an exception is return value sent back to the caller of the message
    * it is not handled the same as other return values but it is one all the
      same
* exceptions do not change the state of the system any more than any other
  return value (the caller can decide what to do with them just like any return
  value)
* any function that can throw an exception has that exception as a return type


### Does sandi metz rules cover invalid and edge cases?

She just recommends the kinds of tests for each category of method - she
doesn't say "write one test case for each of these"

### @searls ruby conf talk

https://www.youtube.com/watch?v=VD51AkG8EZw


From the perspective of prevention he discusses

####  test structure

* too big to fail: tests make big objects harder to manager
    * big objects => big tests
    * many dependencies => hard to do test setup
    * multiple side-effects and return values => lots of verifications
    * many logical branches => have to write many test cases
    * rule of product: the total no. of test cases you have to write is the no. of possible values of each arg *multiplied* together
        ```
        foo(a, b, c)

        Na = no. of possible values of a
        Nb = ...

        num test cases = Na * Nb * Nc
        ```
    * if you are trying to get into testing you first have to get into small objects
    * he limits new object to one public method and 3 dependencies
* every test does the same three things
    1. set stuff up (given)
    2. invoke (when)
    3. verify behaviour and/or state (then)
    * every test is a program that does the same stuff
* rspec
    * let are in the "given" section
    * before could be either given or when
* he puts an empty line between each given/when/then section in every xUnit style test he writes
* he recommends writing tests in a given-when-then conscious way
* smells
    * lots of given steps => object may is hard to setup
    * more than one when step => maybe API is confusing or hard to invoke/understand
    * many then steps => code is doing too much or returning too complex a type
* tests are untested code so test code logic means you spend time trying to figure out the logic not see what it is testing
* he has a squint test
    * can I easily see what is under test
    * are all the methods tested?
    * are they in order?
    * arrange - act - assert should "pop"
* he uses rspec context to point out each logical branch of the method
* tests that are too magic
    * testing libs exist along a spectrum
    * small API, not many features, quicker to learn <--------> large API, lots of features, takes longer to learn
    * aka mintest vs rspec
    * bigger API
        * ++ terse test
        * -- takes longer to learn, looks like magic to new people
    * smaller API
        * -- more "one-off" test helpers
        * ++ quicker (easier?) to learn
* he always calls the thing under test `subject` and always call the thing he will do assertions on `result` or `results`
* consistency is good
    * if you are consistent in your test suite, any inconstency can convey meaning to the reader
    * if your test suite is inconsistent then readers have to read *everything* carefully
* make unimportant test code look obviously silly to the reader
    * test data should be minimal but also minimally meaningful

#### test isolation

* unfocused test suites
    * most teams define a sccuessful test suite as "is it tested"
    * he defines a successful test suite by
        1. is the purpose of each test readily apparent
        1. does the test suite promote consistency?

* he creates a separate suite for each "kind" of test in the suite
    * each suite has its own conventions and consistency
    * creates custome spec helpers for the suite to enforce and encourage that consistency
* TODO http://blog.testdouble.com/posts/2014-05-25-breaking-up-with-your-test-suite.html
* thinking about the agile testing pryamid
    * top = tests are more integrated
    * bottom = tests are less integrated
* a test suite should stay at a consistent height on the pyramid
* be consistent in
    * does this test suite fake 3rd party APIs or not?
    * does this test suite fake out all collaborators or not?
    * does this test suite use the UI or not?
* he recommends starting with two suites
    1. as integrated as we can manage
        * fake as little as we can
        * use UI if it is available
    2. as isolated as possible
    * it sets up two extremes for when he is deciding which suite to put a test in rather than oscillating in-between
* as need arsies you might need to make more suites in the middle but you need to agree on the conventions of the suite before you do it

* tests can be too realistic
    * some people have a goal of their tests being maximally realisistic
    * any "realistic" test has implicit/hidden boundaries on how realisitic it can be e.g. does the test use your production DNS, CDN etc.
    * in reality "real" is impossible to achieve
    * then when something fails, folks check against their goal of "maximum realism" and their answer is to increase the "realism" aka "integratedness" of the tests.
    * realisitc tests have real costs:
        * slower
        * take more time to write, change, debug
        * can fail for reasons other than the code failed
        * more cognitive load to understand them
    * if the test suite has really clear boundaries (saying what it does and does not test) then you can focus on what is tested and what is controlled
    * then when stuff breaks in production
        * team can have a conversation about the trade-offs of why a test didnt' cover that case
        * *maybe* make a separate test (maybe a new suite) to cover that case)
        * don't have to do things that push even harder for the unreachable "fully realistic" goal at an unacceptable cost
    * realism in tests is not an ideal or virtue
        * isolated tests give us much better design feedback

* redundant code coverage
    * redundant test coverage can really kill a team morale
    * you can see the reduendant coverage from the "avg hits/line" column in the coverage
    * outside-in TDD can help with this (use lots of mocks)
        * TODO http://blog.testdouble.com/posts/2015-09-10-how-i-use-test-doubles.html
* careless mocking
    * he uses mocks for API discovery (fake all dependencies that don't exist yet)
    * he strong advises
        * not using a mix of real and fake objects
        * don't use test doubles just for the few objects that are hard to setup
    * haphazard mocking
        * confuses the reader
        * treat the symptom of test pain not the cause
            * if some of your objects are hard to setup that is a symptom not the cause - try to treat the cause by fixing those objects
* app frameworks
    * app frameworks provide a lot of "integration level concerns" - they help you plug you domain models into the rest of the world
    * frameworks focus on integration problems
    * framewroks test helpers are very integrated
    * if you look at the framework as the "giver of all things I need" then we end up writing all integration tests
    * he suggests having a suite that couples to the framework and one that does not
        * rspec encourages this with rails these days with `spec_helper.rb` vs `rails_helper.rb`

#### test feedback

* bad error messages waste time when tests fail
    * error output is a very important feature of assertion libraries
* slow loops
    * recommends monitoring my own feedback loop
* painful test data
    * there are different levels of "control" of test data
        * inline
            * good for models
        * fixtures
            * good for integration tests
        * data dump
            * good for smoke tests
            * can be faster than factories
        * "self priming tests"
            * you have to use the app UI to get the app into a state that you can run the test
            * you have to do this if you need to test something in production
* superlinear build slowdown
    * the time a single test takes to run is
        * app time + test time + setup and teardown time
    * as the app gets bigger, app time and setup/teardown time will too so the time taken to run the same test will increase even though that test has not changed at all!
        * he recommends monitoring build time
        * also can cap build time and when the suite approaches it delete or refactor tests to stay within it
    * avoid the urge to create a new integration test for each feature
        * try to have integration tests that zig-zag through features - this is more like what users do anyway
* false negatives
    * what does it mean when a build fails?
        * waht file needs to change to fix the build?
            * a true negative: red build means the code was broken
                * depressingly rare
                * increase confidence in our tests
            * a false negative: red build means we forgot to update a test
                * depressingly common
                * errode confidence in our tests
    * causes of false-negative failures:
        1. slow tests
            * tests were too slow to run before I pushed
        2. redundant tests
            * i changed some code but lots of tests depended on it
    * track whether each build failure is a true or false negative and how long it took you to fix it

#### Summary

Test suites without a plan are problematic - Team should agree on some choices
for their suites:

* discuss the defnintion of a successful test suite:
    1. is the purpose of each test readily apparent
    1. does the test suite promote consistency?
* discuss and agree on the boundaries of "realisim" in our tests and what we are willing to accept
* agree on how we use mocking
    * don't have a mix of mocks and real objects
    * don't use mocks as a way to avoid difficult to setup objects - that is fixing the symptom, not the cause of test pain
    * mock or real all 3rd party APIs?
    * mock or real all collaborator objects?
* don't be haphazard with mocking - have a plan
* decide how many suites we should have and what level of isolation they have
* set a maximum build time and agree we will refactor tests/delete them to keep within it
* only use `rails_helper` when you really need it
* agree not to create an integration test for each feature
    * agree to try and have integration tests that cover a lot of ground - maybe like a real user would

things about beemo test suite
* we use rails_helper everywhere
* we don't have an agreement on mocking/levels of isolation
* we have some controller tests - should they be more integrated?
* our suites are organised by type not by level of isolation



## http://blog.testdouble.com/posts/2014-05-25-breaking-up-with-your-test-suite.html

> Kent beck"
> I get paid for code that works. I test as little as possible to reach a given level of confidence.

* Testing is not an end in itself
* Neither is coding, coding is not an end in itself
    * the "carrying cost" of code is high - if you can solve a clients problem without code you probably should

Questions to ask yourself

* Is TDD right in this context?
* Is Rails right for this job?
* Is ruby?

There are many things tests can *possibly* help with and the break down into two categories

1. understanding - value comes from writing test
2. confidence - value comes from running test

* understanding (value is in the *writing* and maintaining the test as it informs the design of the system)
    1. validate code is useful to others
    1. grow a maintainable private API
    1. make the public API easy to use
    1. narrowly specify our dependencies (???)
    1. keep the product simple
    1. design a good API for each object
* confidence (value is in *running* tests to tell you whether things are good or not)
    1. verify that production is working
    1. safeguard our use of 3rd party code
    1. know that my code works
    1. ensure behaviour of others services
    1. safely change and refactor

BUT no one test can do all of these things at the same time - single responsibility testing!

=> you will have tests that serve a no. of roles in the codebase

1. Bugfixing tests that are more integrated
1. exploratory tests that were used during design to decide on the API by mocking out all collaborators

* @searls thinks that it should be clear what kind of test we are looking at before we open the file
* unclear tests
    * cost money because none of the things below have value but they do take time
    * have unclear rules (about mocks etc.)
    * constantly rediscivering the "purpose" of the test
    * rules are debatable
    * structure is ad-hoc
        * do we organise by methods or behaviour or something else?

Every test suite should promote at most exactly one type of confidence and one type of understanding

=> it should be obvious from the file path and name what the 1) confidence and 2) understanding is for the test file I am about to open

### Suite 1: safety test suite

All of these mean roughly the same thing:

* Smoke test
* Acceptance tests
* Feature test
* End-to-end test

@searls pulled them out into the pithy SAFE acronym
The SAFE test suite is the suite which provides safety!

* the user = real world user as possible
    * HTTP API endpoint => api client
    * HTML endpoint => some sort of web automation e.g. selenium, capybara etc.
* confidence
    * all the layers of the application work
* understanding
    * how simple is our product?
        * if you can't run all the possible things a user might want to do in the app in under 30 mins the app is *complex*
        * if you can't add a new safety teset to the app then it might be *complicated*
* guidelines
    * they shouldn't know about internal APIs
        * ideally bind to user visible stuff not markup or DB changes
    * enforce a time budget on the safety test suite because it will get slow
* warning signs
    * failures due to refactors are false negatives
    * human intuition tends to value overly realistic tests
    * superlinear slowdown
        * more tests => slower build
        * as system gets bigger each individual test will run slower

* safe tests are accurate but also expensive

### Suite 2: consumption test suite

* user:
    * whoever is using our thing where thing can be a service, an API endpoint, a page on the site etc.
* confidence:
    * verify the behaviour we are directly responsible for
* understanding:
    * is our API easy to use
    * if it is hard to write a test it is probably hard to use
* guidelines
    * module boundaries should be meaningful beyond testing
    * fake all external depencencies
    * exercise public APIs not private APIs
    * organise by consumer's desired outcomes not class names
* warning signs:
    * if you are using faked services these should not be slow!
        * if they are slow then you should fix it
    * this suite is you refactoring protection suite
        * if it is not catching refactoring errors you should fix it


Inter-service tests (tests between two services) are tempting but they are

* hard to setup
* slow to run
* they represent redundant test coverage

The root cause is a lack of trust that the other services will continue to work

@searls says to default to trusting the other services (whether they are
managed by your team or another team) on which you depend. When you can't
ttrust the other service, write contract tests (see below).

## Contract test suite

Write a test that represents your interests in somebody else's repo and have
your contact details on the test so they can tell you if that test breaks. This allows you to discuss the problem before it hits production

Useful for

* if you are using an API in an unusual or unsupported way i.e. you think the
  maintainers might not be testing your use-case properly

* user:
    * you!
* confidence:
    * dependencies will continue to work the way we need them to
* understanding:
    * learn whether that service continues to be a good fit for our needs
    * if it keeps breaking then we might have a problem and we get to make that decision *before* it breaks in production
* guidelines
    * written just like consumption tests
    * commited into the repo they are testing
    * maintainers of that repo can provide some setup and teardown convenience helpers
* warning signs:
    * it is not a replacement for human interaction! Your first reaction to a service not working as you need is to go talk to the people who maintain it.[:w

None of the tests above provide good feedback about the design of our code

@searls thinks TDD is about discovery

* discover of API
* discovery of tiny, boring, consistent units of code that break a big problems into small manageable ones

## Adapter test suite

* Mocking what you don't own leads to useless pain!
* instead wrap 3rd party APIs with tiny objects that you do own
* adapters should be thin!
    * often thin enough not to need tests

* user
    * your application trying to udnerstand its relationship to a 3rd party API
    * exercise the 3rd party API under realistic circumstances
* confidence
    * tests of libraries warn when an upgrade might be unsafe
    * for network services it can alert us to outages and breaking changes
* understanding
    * serve as specs for how we use the 3rd party thing
    * drastically reduces the cost of replacement later
    * easy to replace one gem with another if our usage of the gem is down to one little adapter object
* guidelines
    * only test adapters when you have good cause
    * by default you shouldn't be testing the framework
    * default to not writing adapter tests
* warning signs
    * can be tricky to run in CI because they depend on 3rd party gem/API
        * might make more sense to run from the safety suite
    * are likely to be slow for reasons you can't control

### Discovery test suite

* user
    * the first person to call a new method
    * they are concerned with the inputs that the new method needs and the outputs AND side-effects it will have
        * they are concerned with inputs and outcomes
    * the discovery test suite is code by wishful thinking
* confidence
    * is limited. The discovery test suite does not provide much confidence that things work
        * it can provide confidence that pure first order functions work
* understanding
    * is rich
    * I'm getting small things or "free SRP"
    * separation of roles
* guidelines
    * command and query objects discover dependencies using test doubles
    * @searls really does make a LOT of small objects, each of which falls into one category
        1. command object
        2. query object
        3. logic object
            * logical tests discover implementation by usage
                * @searls recommends NOT using test doubles for logical tests
    * He tries to limit objects to one public method and 3 private methods
    * command and query objects contain very little logic and do use test doubled to discover collaborator interfaces
    * test pain is good in this stage as it indicates problems with your code design
* warning signs
    * yield small and *disposable* units
        * don't be afraid to throw away those small units
    * re-use is overrated - this model of work creates a lot of specialised things that don't lend themselves particularly to re-use
    * extract refactors in this model of work are a smell - instead destroy the old thing and make a new thing
        * it should not be necessary if we are preemptively pushing out small things
    * frameworks will fight you
        * frameworks and TDD try to solve the same problem
            * frameworks provide a structure and types for you to use
            * TDD wants you to discover that set of types for yourself

Only the command and query objects in the discovery test suite should use mock objects!

## Summary

All of the above is just one way to break up into multiple test suites

* safety suite
* contract suite
* consumption suite
* discovery suite
* adapter suite

## What we have by default in rails apps

`spec/features`
    * seems to map to a mix of the safety and consumption suites here
    * -- a mix of slow safety tests and fast consumption tests
    * -- no clear mocking policies (3rd party api, collaborator)
`spec/models` suite
    * seems to map to a mix of discovery + adapter + consumption
    * -- no clear mocking policies (3rd party api, collaborator)

## Criteria for categorising a test suite

* XYZ suite
    * user:
    * confidence:
    * understanding:
    * organised by: class name|customer behaviour
    * 3rd party API policy: all real|all mock
    * collaborator object policy: all real|all mock
    * guidelines:
        add guidelines to help us get this right
    * warning signs:
        add warning signs that we might not be sticking to the policy
    * limits of its realism
        agree on what this suite does not try to address




* models suite
    * user:
        * client code of each model
    * confidence:
    * understanding:
        * how easy is each model to setup
    * guidelines:
    * warning signs:
        * no clear mocking policy
    * organised by: class name
    * 3rd party API policy: sometimes real, sometimes mocks
    * collaborator object policy: sometimes real, sometimes mocks
* features suite
    * user:
    * confidence:
    * understanding:
    * guidelines:
        * organised by:
    * warning signs:
* requests suite
    * user:
    * confidence:
    * understanding:
    * guidelines:
    * warning signs:
* acceptance suite
    * user:
    * confidence:
    * understanding:
    * guidelines:
    * warning signs:
* background jobs suite
    * user:
    * confidence:
    * understanding:
    * guidelines:
    * warning signs:
* rake tasks suite
    * user:
    * confidence:
    * understanding:
    * guidelines:
    * warning signs:

* usually ignored
    * controllers suite
    * routes suite
    * helpers suite
    * views suite

# http://blog.testdouble.com/posts/2015-09-10-how-i-use-test-doubles.html

* Bottom up TDD requires you to come up with your first design on your own
    * ++ it does help you implement that design
    * -- it kinda requires you to have a good design in your head before you start
        * this is possibly why it works so well for experienced devs
* Top down TDD
    * tries to help you come up with the design as well as implement it

write the first top-level test
    ? should that be a controller or acceptance test?
    ? is it a safety test or a discovery test? is it quite integrated?
    when you do find an object you need a sense of whether it is collaborator or logic object?

@searls objects tend to fall into

1. collaborator objects
    * contain very little logic
    * they gather input for the logic objects
    * they pipeline output from the logic objects
    * use test doubles to discover their dependencies
    * in functional lang they are
    * they don't hold any state about the transaction in progress - the value objects do that
2. logic objects
    * tend to be pure functions
    * "logical leaf node"
    * because they are pure functions we don't need test doubles
2. value objects
    * wrap data in the application
    * they are the data that flows through the pipes we created (pipes of collaborator and logic objects)
    * provide methods that expose that data
    * these don't use test doubles
4. wrapper objects
    * it wraps a 3rd party API
    * unit tests not that useful - @searls doesn't write unit tests of these
    * he uses the safety net integration tests to catch issues

When he has to make big changes he favours rewrites i.e. don't try to make an
existing small object od something new, instead created a new small object and
change the plumbing

Pros/cons of "Discovery testing"

* intense design pressure yields small focused units
* ++ every method signature and type is validated before it exists - very little waste
* refactoring becomes an exceptional case
* -- much complex to learn that than red-green-refactor
* -- radical implementation changes favour resrites over refactor
    * tests are tightly coupled to the implementation
* -- collaborator tests value is front-loaded
    * they tend not to be valued by folks who don't practice this workflow

Most test double libs tend to be better at one style of TDD than another.
Older larger libs tesnd to be unopinionated which makes them confusing for new folks

### VerbNoun.verb naming

@searls creats objects in verbNoun form not NounVerb e.g.

GeneratesSeedWorld vs SeedWorldGenerator
BuildsTransaction vs TransactionBuilder

his stated reasons are
1. the verb is what matters
1. over time if the verb stays the same but the noun generalizes you end up with slightly cleaner design

he tends to have the main (only?) public method of each of the VerbNoun objects match the verb

BuildsTransaction.build
GeneratesSeedWorld.generate

## How I think about objects

I think I need to get away from objects being nouns/people
they can also be processes! VerbNoun naming might be a good way to break that habit


## Things to try in my own process

1. try to think about objects in the collaborator, logic, value categories

2. have a box drawing desgin step where I try to break the problem into an object tree
    * collaborator (input and output) and logical objects go in the tree
    * have a separarate area for value objects (they get passed around)

    reasons I don't do this at the moment:
        * i feel like i own't be able to see what objects I need until I have some code
            * this might not be true - experiment!

