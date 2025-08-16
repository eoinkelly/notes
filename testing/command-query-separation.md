# Command query separation and testing

Command & query separation is another way of saying you should be aware of which
methods have side-effects and which do not!

- ruby has implicit returns so it is natural to make command methods also return
  the result of their computation
- there is a value in being aware of the difference between the two types of
  message but it is not always possible or desirable to have only methods that
  only do commands or queries

Understanding the difference between command and query messages is important BUT
what value does writing code in a style that keeps them separate bring?

- possible answers:
    - tests are shorter - they only have to test
    - ++ encourages you to keep your side-effecting code separate
    - ++ you end up with more pure functions which are easy to understand/test

### command query separating in functional style

In a strict functional style

- if you call a pure function which calls a bunch of other pure functions you
  can treat it like a query message because it is idempotent and does not change
  the state of the system
- => there are no "command" messages if your system uses pure functions =>
  testing is simple - just test incoming public functions and assert on their
  return values

An outgoing command message is kind of "change the state of the wider system in
XYZ way"

The sending of the outgoing command is a "state change" in the system - it may
not be the full story of that state change but it is the bit that is releveant
to the object under test

If the outgoing command is "change the database" they may be many objects
between your test subject and the end side-effect. We don't want to test all of
those - we only want to test the bit of this "state change" that is part of our
test subject i.e. the outgoing command message

Tests that test distant side-effects are integration tests not a unit test!

# Query messages

- query messages should be idempotent
- collolory: any method which is idempotent (i.e. a pure function) is a query
- a "query" message is another name for a pure function
- both outgoing and incoming query messages are idempotent
- you can send an outgoing query message `0->N` times and the state of the app
  will be unchanged so in a way outgoing query messages are "invisible"

Testing incoming messages

- incoming query => assert return value is expected
- incoming command
    - assert direct, public, local side effects only
    - i.e. in the test you call the method and check some other bit of the
      subject's public interface to see if the correct things changed

# Command messages

- a message is "command" if it has side-effects (by defn)
- incoming command message will do at one to all of
    - maybe change the state of the subject
    - maybe send outgoing command messages to other objects (which will change
      their state)
- if an incoming command message does not change the state of something in the
  system it is NOT a command message!

# What to test

private query => do not test private command => do not test

outgoing query => do not test outgoing command => assert message sent

sometimes you have to have a method that does both command and query the point
is to know what you have as they are tested differently

```
class Car
  def initialize
  end

  # incoming query
  # the test will make a new instance of this object, call this method and
  # assert that the return value is expected
  def position
  end

  # incoming command
  def do_public_thing
    wheel = suspension.wheel # outgoing query (not tested), hits a stub in test
    wheel.turn! # outgoing command (assert that call happened with a mock)
  end

  private
  # do not test
  def do_thing_1
  end
end
```
