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

