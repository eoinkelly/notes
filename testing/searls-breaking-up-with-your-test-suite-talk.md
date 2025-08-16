## http://blog.testdouble.com/posts/2014-05-25-breaking-up-with-your-test-suite.html

> Kent beck" I get paid for code that works. I test as little as possible to
> reach a given level of confidence.

- We are paid for working features for clients
- We are not paid to write code
- Testing is not an end in itself
    - tests have a maintenance/carrying cost so write as few as you can to reach
      a **given** level of confidence
        - => it is important to define that level of confidence well
- Neither is coding, coding is not an end in itself
    - the "carrying cost" of code is high - if you can solve a clients problem
      without code you probably should

- We are too likely to prioritize code over working features
    - Questions to ask yourself
        - Is TDD right in this context?
        - Is Rails right for this job?
        - Is ruby?

There are many things tests can _possibly_ help with and they break down into
two categories

1. understanding - value comes from writing test
2. confidence - value comes from running test

Going deeper into these categories

- types of understandings (the value is in the _writing_ and maintaining the
  test as it informs the design of the system)
    1. validate code is useful to others
    1. grow a maintainable private API
    1. make the public API easy to use
    1. narrowly specify our dependencies (???)
    1. keep the product simple
    1. design a good API for each object
- types of confidences (the value is in _running_ tests to tell you whether
  things are good or not)
    1. verify that production is working
    1. safeguard our use of 3rd party code
    1. know that my code works
    1. ensure behaviour of others services
    1. safely change and refactor

BUT no one test can do all of these things at the same time - single
responsibility testing!

=> not all your tests have the same goals!

Some examples

1. Bugfixing tests that are more integrated
1. exploratory tests that were used during design to decide on the API by
   mocking out all collaborators

@searls thinks that it should be clear what kind of test we are looking at
before we open the file

Unclear tests:

- cost money because none of the things below have value but they do take time
- have unclear rules (about mocks etc.)
- constantly rediscivering the "purpose" of the test
- rules are debatable
- structure is ad-hoc

Every test suite should promote at most exactly one type of confidence and one
type of understanding

=> it should be obvious from the file path and name what the 1) confidence
and 2) understanding is for the test file I am about to open

### Suite 1: safety test suite

All of these mean roughly the same thing:

- Smoke test
- Acceptance tests
- Feature test
- End-to-end test

@searls pulled them out into the pithy SAFE acronym The SAFE test suite is the
suite which provides safety!

- the user = real world user as possible
    - HTTP API endpoint => api client
    - HTML endpoint => some sort of web automation e.g. selenium, capybara etc.
- confidence
    - all the layers of the application work
- understanding
    - how simple is our product?
        - if you can't run all the possible things a user might want to do in
          the app in under 30 mins the app is _complex_
        - if you can't add a new safety test to the app then it might be
          _complicated_
- guidelines
    - they shouldn't know about internal APIs
        - ideally bind to user visible stuff not markup or DB changes
    - enforce a time budget on the safety test suite because it will get slow
- warning signs
    - failures due to refactors are false negatives
    - human intuition tends to value overly realistic tests
    - superlinear slowdown
        - more tests => slower build
        - as system gets bigger each individual test will run slower

- safe tests are accurate but also expensive

### Suite 2: consumption test suite

- user:
    - whoever is using our thing where thing can be a service, an API endpoint,
      a page on the site etc.
- confidence:
    - verify the behaviour we are directly responsible for
- understanding:
    - is our API easy to use
    - if it is hard to write a test it is probably hard to use
- guidelines
    - module boundaries should be meaningful beyond testing
    - fake all external depencencies
    - exercise public APIs not private APIs
    - organise by consumer's desired outcomes not class names
- warning signs:
    - if you are using faked services these should not be slow!
        - if they are slow then you should fix it
    - this suite is you refactoring protection suite
        - if it is not catching refactoring errors you should fix it

Inter-service tests (tests between two services) are tempting but they are

- hard to setup
- slow to run
- they represent redundant test coverage

The root cause is a lack of trust that the other services will continue to work

@searls says to default to trusting the other services (whether they are managed
by your team or another team) on which you depend. When you can't trust the
other service, write contract tests (see below).

## Contract test suite

Write a test that represents your interests in somebody else's repo and have
your contact details on the test so they can tell you if that test breaks. This
allows you to discuss the problem before it hits production

Useful for

- if you are using an API in an unusual or unsupported way i.e. you think the
  maintainers might not be testing your use-case properly

- user:
    - you!
- confidence:
    - dependencies will continue to work the way we need them to
- understanding:
    - learn whether that service continues to be a good fit for our needs
    - if it keeps breaking then we might have a problem and we get to make that
      decision _before_ it breaks in production
- guidelines
    - written just like consumption tests
    - commited into the repo they are testing
    - maintainers of that repo can provide some setup and teardown convenience
      helpers
- warning signs:
    - it is not a replacement for human interaction! Your first reaction to a
      service not working as you need is to go talk to the people who maintain
      it.

None of the tests above provide good feedback about the design of our code

@searls thinks TDD is about discovery

- discover of API
- discovery of tiny, boring, consistent units of code that break a big problems
  into small manageable ones

## Adapter test suite

- Mocking what you don't own leads to useless pain!
- instead wrap 3rd party APIs with tiny objects that you do own
- adapters should be thin!
    - often thin enough not to need tests

- user
    - your application trying to udnerstand its relationship to a 3rd party API
    - exercise the 3rd party API under realistic circumstances
- confidence
    - tests of libraries warn when an upgrade might be unsafe
    - for network services it can alert us to outages and breaking changes
- understanding
    - serve as specs for how we use the 3rd party thing
    - drastically reduces the cost of replacement later
    - easy to replace one gem with another if our usage of the gem is down to
      one little adapter object
- guidelines
    - only test adapters when you have good cause
    - by default you shouldn't be testing the framework
    - default to not writing adapter tests
- warning signs
    - can be tricky to run in CI because they depend on 3rd party gem/API
        - might make more sense to run from the safety suite
    - are likely to be slow for reasons you can't control

### Discovery test suite

- user
    - the first person to call a new method
    - they are concerned with the inputs that the new method needs and the
      outputs AND side-effects it will have
        - they are concerned with inputs and outcomes
    - the discovery test suite is code by wishful thinking
- confidence
    - is limited. The discovery test suite does not provide much confidence that
      things work
        - it can provide confidence that pure first order functions work
- understanding
    - is rich
    - I'm getting small things or "free SRP"
    - separation of roles
- guidelines
    - command and query objects discover dependencies using test doubles
    - @searls really does make a LOT of small objects, each of which falls into
      one category
        1. command object
        2. query object
        3. logic object
            - logical tests discover implementation by usage
                - @searls recommends NOT using test doubles for logical tests
    - He tries to limit objects to one public method and 3 private methods
    - command and query objects contain very little logic and do use test
      doubled to discover collaborator interfaces
    - test pain is good in this stage as it indicates problems with your code
      design
- warning signs
    - yield small and _disposable_ units
        - don't be afraid to throw away those small units
    - re-use is overrated - this model of work creates a lot of specialised
      things that don't lend themselves particularly to re-use
    - extract refactors in this model of work are a smell - instead destroy the
      old thing and make a new thing
        - it should not be necessary if we are preemptively pushing out small
          things
    - frameworks will fight you
        - frameworks and TDD try to solve the same problem
            - frameworks provide a structure and types for you to use
            - TDD wants you to discover that set of types for yourself

Only the command and query objects in the discovery test suite should use mock
objects!

## Summary

All of the above is just one way to break up into multiple test suites

- safety suite
- contract suite
- consumption suite
- discovery suite
- adapter suite

## What we have by default in rails apps

- `spec/features`
    - seems to map to a mix of the safety and consumption suites here
    - -- a mix of slow safety tests and fast consumption tests
    - -- no clear mocking policies (3rd party api, collaborator)
- `spec/models` suite
    - seems to map to a mix of discovery + adapter + consumption
    - -- no clear mocking policies (3rd party api, collaborator)

## Criteria for categorising a test suite

- XYZ suite
    - user:
    - confidence:
    - understanding:
    - organised by: class name/customer behaviour
    - 3rd party API policy: all real/all mock
    - collaborator object policy: all real/all mock
    - guidelines: add guidelines to help us get this right
    - warning signs: add warning signs that we might not be sticking to the
      policy
    - limits of its realism agree on what this suite does not try to address

Examples of test suites from standard rails apps

- models suite
    - user:
        - client code of each model
    - confidence:
    - understanding:
        - how easy is each model to setup
    - guidelines:
    - warning signs:
        - no clear mocking policy
    - organised by: class name
    - 3rd party API policy: sometimes real, sometimes mocks
    - collaborator object policy: sometimes real, sometimes mocks
- features suite
    - user:
    - confidence:
    - understanding:
    - guidelines:
        - organised by:
    - warning signs:
- requests suite
    - user:
    - confidence:
    - understanding:
    - guidelines:
    - warning signs:
- acceptance suite
    - user:
    - confidence:
    - understanding:
    - guidelines:
    - warning signs:
- background jobs suite
    - user:
    - confidence:
    - understanding:
    - guidelines:
    - warning signs:
- rake tasks suite
    - user:
    - confidence:
    - understanding:
    - guidelines:
    - warning signs:

The following suites are usually ignored by rails devs these days

- controllers suite
- routes suite
- helpers suite
- views suite
