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

* suite without a plan are problematic

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
