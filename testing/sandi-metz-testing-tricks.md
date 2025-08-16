# Magic tricks of unit testing

From the point of view of a single object there are three sources of messages

1. messages that come from myself
2. messages I send to other objects (outgoing messages)
3. messages that come from other objects (incoming messages

There are two kinds of message

1. command message (return value ignored, has side-effects)
2. query message (returns a value, no side-effects aka pure function)

so there are 3 x 2 = 6 different kinds of message

The grid

- Incoming query: assert return value
- Incoming command: assert **direct**, **public** side effects

- Incoming private message: don't test
- Outgoing private message: don't test

- Outgoing query message: don't test (they will be tested as incoming by the
  receiver)
- Outgoing command message: assert that the mesage was sent

## aside: sandi metz rules

Clarifying question: for each message, is this message a command, query or both?
where are the query tests that assert ret val of the message where are te
command tests taht assert state of world after message send

- when testing the outgoing messages that result from an incoming message I
  should test them even if they originate from a private method that was
  triggered by the public method
    - that detail is just an implementation detail - from outside we can
      comceptualise of the method being completedly inline
- should test _all possible outgoing messages_

What are the actions my method can take on the system as a whole

1. do nothing to the state of the system (query method)
2. send command messages to collaborators
    - included in those messages are the exceptions it can raise
    - included in these messages is "changing the state of collaborators" (which
      we can only do via messages)
3. send query messages to collaborators - ignore.

### What are exceptions (hint: return values)

- An exception is the argument to the special "there was an exception" message
  that is sent to each method in the current callstack in turn.
- So an exception is return value sent back to the caller of the message
    - it is not handled the same as other return values but it is one all the
      same
- exceptions do not change the state of the system any more than any other
  return value (the caller can decide what to do with them just like any return
  value)
- any function that can throw an exception has that exception as a return type

### Does sandi metz rules cover invalid and edge cases?

She just recommends the kinds of tests for each category of method - she doesn't
say "write one test case for each of these"
