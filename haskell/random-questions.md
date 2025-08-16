# Why is if-else ok in Haskell but not in OO langs?

In OO we are encouraged to never use if-else but in haskell there is syntax for
a giant switch statement at the top level of evyer function. These seem to be at
odds.

Reasons if-else is problematic in OO languages:

1. higher branching complexity in methods
1. It is considered bad if a method has to be defensive and check that a
   particular argument supports the messages the method wants to send it.

# random, not very clear thoughts

- Haskell functions allow you to have a separate function body for based on the
  values of your args
- you can also break apart non scalar args to match in the values that make them
  up e.g. break up a tuple

In OO we are encouraged to never have a different code path in a function based
on the type of an argument. In effect we are to treat each argument as always
having the same "type" (where type is just the collection of messages you can
send that thing)

=> so we can express the OO thing as "each function should be always be able to
send the messages it wants to to each argument - it is not the functions
responsibility to know whether it can send the message or not.

In haskell the type system (theoretically) makes sure that a function cannot do
something illegal with an argument so you shouldn't ever need to check the type
of it - does it work out in practice?

The haskell type system allows/forces you to constrain what types a function's
args can be also there is no notion of "sending messages" in Haskell
