# How to stop hating your test suite by @searls

https://www.youtube.com/watch?v=VD51AkG8EZw

From the perspective of prevention he discusses

#### test structure

- too big to fail: tests make big objects harder to manager
    - big objects => big tests
    - many dependencies => hard to do test setup
    - multiple side-effects and return values => lots of verifications
    - many logical branches => have to write many test cases
    - rule of product: the total no. of test cases you have to write is the no.
      of possible values of each arg _multiplied_ together

        ```
        foo(a, b, c)

        Na = no. of possible values of a
        Nb = ...

        num test cases = Na * Nb * Nc
        ```

    - if you are trying to get into testing you first have to get into small
      objects
    - he limits new object to one public method and 3 dependencies

- every test does the same three things
    1. set stuff up (given)
    2. invoke (when)
    3. verify behaviour and/or state (then)
    - every test is a program that does the same stuff
- rspec
    - let are in the "given" section
    - before could be either given or when
- he puts an empty line between each given/when/then section in every xUnit
  style test he writes
- he recommends writing tests in a given-when-then conscious way
- smells
    - lots of given steps => object may is hard to setup
    - more than one when step => maybe API is confusing or hard to
      invoke/understand
    - many then steps => code is doing too much or returning too complex a type
- tests are untested code so test code logic means you spend time trying to
  figure out the logic not see what it is testing
- he has a squint test
    - can I easily see what is under test
    - are all the methods tested?
    - are they in order?
    - arrange - act - assert should "pop"
- he uses rspec context to point out each logical branch of the method
- tests that are too magic
    - testing libs exist along a spectrum
    - small API, not many features, quicker to learn <--------> large API, lots
      of features, takes longer to learn
    - aka mintest vs rspec
    - bigger API
        - ++ terse test
        - -- takes longer to learn, looks like magic to new people
    - smaller API
        - -- more "one-off" test helpers
        - ++ quicker (easier?) to learn
- he always calls the thing under test `subject` and always call the thing he
  will do assertions on `result` or `results`
- consistency is good
    - if you are consistent in your test suite, any inconstency can convey
      meaning to the reader
    - if your test suite is inconsistent then readers have to read _everything_
      carefully
- make unimportant test code look obviously silly to the reader
    - test data should be minimal but also minimally meaningful

#### test isolation

- unfocused test suites
    - most teams define a sccuessful test suite as "is it tested"
    - he defines a successful test suite by
        1. is the purpose of each test readily apparent
        1. does the test suite promote consistency?

- he creates a separate suite for each "kind" of test in the suite
    - each suite has its own conventions and consistency
    - creates custome spec helpers for the suite to enforce and encourage that
      consistency
- TODO
  http://blog.testdouble.com/posts/2014-05-25-breaking-up-with-your-test-suite.html
- thinking about the agile testing pryamid
    - top = tests are more integrated
    - bottom = tests are less integrated
- a test suite should stay at a consistent height on the pyramid
- be consistent in
    - does this test suite fake 3rd party APIs or not?
    - does this test suite fake out all collaborators or not?
    - does this test suite use the UI or not?
- he recommends starting with two suites
    1. as integrated as we can manage
        - fake as little as we can
        - use UI if it is available
    2. as isolated as possible
    - it sets up two extremes for when he is deciding which suite to put a test
      in rather than oscillating in-between
- as need arsies you might need to make more suites in the middle but you need
  to agree on the conventions of the suite before you do it

- tests can be too realistic
    - some people have a goal of their tests being maximally realisistic
    - any "realistic" test has implicit/hidden boundaries on how realisitic it
      can be e.g. does the test use your production DNS, CDN etc.
    - in reality "real" is impossible to achieve
    - then when something fails, folks check against their goal of "maximum
      realism" and their answer is to increase the "realism" aka
      "integratedness" of the tests.
    - realisitc tests have real costs:
        - slower
        - take more time to write, change, debug
        - can fail for reasons other than the code failed
        - more cognitive load to understand them
    - if the test suite has really clear boundaries (saying what it does and
      does not test) then you can focus on what is tested and what is controlled
    - then when stuff breaks in production
        - team can have a conversation about the trade-offs of why a test didnt'
          cover that case
        - _maybe_ make a separate test (maybe a new suite) to cover that case)
        - don't have to do things that push even harder for the unreachable
          "fully realistic" goal at an unacceptable cost
    - realism in tests is not an ideal or virtue
        - isolated tests give us much better design feedback

- redundant code coverage
    - redundant test coverage can really kill a team morale
    - you can see the reduendant coverage from the "avg hits/line" column in the
      coverage
    - outside-in TDD can help with this (use lots of mocks)
        - TODO
          http://blog.testdouble.com/posts/2015-09-10-how-i-use-test-doubles.html
- careless mocking
    - he uses mocks for API discovery (fake all dependencies that don't exist
      yet)
    - he strong advises
        - not using a mix of real and fake objects
        - don't use test doubles just for the few objects that are hard to setup
    - haphazard mocking
        - confuses the reader
        - treat the symptom of test pain not the cause
            - if some of your objects are hard to setup that is a symptom not
              the cause - try to treat the cause by fixing those objects
- app frameworks
    - app frameworks provide a lot of "integration level concerns" - they help
      you plug you domain models into the rest of the world
    - frameworks focus on integration problems
    - framewroks test helpers are very integrated
    - if you look at the framework as the "giver of all things I need" then we
      end up writing all integration tests
    - he suggests having a suite that couples to the framework and one that does
      not
        - rspec encourages this with rails these days with `spec_helper.rb` vs
          `rails_helper.rb`

#### test feedback

- bad error messages waste time when tests fail
    - error output is a very important feature of assertion libraries
- slow loops
    - recommends monitoring my own feedback loop
- painful test data
    - there are different levels of "control" of test data
        - inline
            - good for models
        - fixtures
            - good for integration tests
        - data dump
            - good for smoke tests
            - can be faster than factories
        - "self priming tests"
            - you have to use the app UI to get the app into a state that you
              can run the test
            - you have to do this if you need to test something in production
- superlinear build slowdown
    - the time a single test takes to run is
        - app time + test time + setup and teardown time
    - as the app gets bigger, app time and setup/teardown time will too so the
      time taken to run the same test will increase even though that test has
      not changed at all!
        - he recommends monitoring build time
        - also can cap build time and when the suite approaches it delete or
          refactor tests to stay within it
    - avoid the urge to create a new integration test for each feature
        - try to have integration tests that zig-zag through features - this is
          more like what users do anyway
- false negatives
    - what does it mean when a build fails?
        - waht file needs to change to fix the build?
            - a true negative: red build means the code was broken
                - depressingly rare
                - increase confidence in our tests
            - a false negative: red build means we forgot to update a test
                - depressingly common
                - errode confidence in our tests
    - causes of false-negative failures:
        1. slow tests
            - tests were too slow to run before I pushed
        2. redundant tests
            - i changed some code but lots of tests depended on it
    - track whether each build failure is a true or false negative and how long
      it took you to fix it

#### Summary

Test suites without a plan are problematic - Team should agree on some choices
for their suites:

- discuss the defnintion of a successful test suite:
    1. is the purpose of each test readily apparent
    1. does the test suite promote consistency?
- discuss and agree on the boundaries of "realisim" in our tests and what we are
  willing to accept
- agree on how we use mocking
    - don't have a mix of mocks and real objects
    - don't use mocks as a way to avoid difficult to setup objects - that is
      fixing the symptom, not the cause of test pain
    - mock or real all 3rd party APIs?
    - mock or real all collaborator objects?
- don't be haphazard with mocking - have a plan
- decide how many suites we should have and what level of isolation they have
- set a maximum build time and agree we will refactor tests/delete them to keep
  within it
- only use `rails_helper` when you really need it
- agree not to create an integration test for each feature
    - agree to try and have integration tests that cover a lot of ground - maybe
      like a real user would
