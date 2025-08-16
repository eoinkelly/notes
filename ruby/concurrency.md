# Concurrency in ruby

## Background

Sources

- https://www.youtube.com/watch?v=dP4U1yI1WZ0

- ruby compiler can re-order or remove code as part of optimizations
- thead safe is not the same as logically correct

- Making an IO/system/backtick call in ruby will block the current thread
    - when a thread is blocked waiting on IO then that thread _cannot_ change
      the internal state of ruby.exe
    - when it blocks ruby knows that thread can't modify ruby's internal state
      so it releases the GIL
        - the GIL protexts ruby.exe internal state, not our code
- => IO in ruby can be asynchonrous

TL;DR ruby is good a concurrent IO but not concurrent CPU core usage

## Concurrency tools in stdlib

Ruby stdlib concurrency tools are basic

- Thread
- Fibre
- Mutex
- ConditionVariable

TODO: re-read stormier book on concurrent ruby

## concurrent-ruby gem

- has a memory model
    - an abstraction over all ruby implementations that the concurrent-ruby gem
      can depend on
    - defines
        1. atomicity
        2. volatility
            - used in the sense of java
            - you cannot get stale reads
        3. serializablitiy of variables

local variables = atomic instance variables = atomic and serializable

concurrent-ruby uses `Concurrent::Synchronization::Object` to make sure the ruby
implements the memory model.

attr_volatile attr_atomic

adds atomic operations

    #compare_and_set_count
    #swap_count

### concurrent-ruby Memory model

Sources

- https://docs.google.com/document/d/1pVzU8w_QF44YzUCCab990Q_WZOdhpKolCIHaiXG-sPw/edit#heading=h.gh0cw4u6nbi5

- Key properties of a memory model are:
    - volatility (V)
        - A written value is immediately visible to any subsequent volatile read
          of the same variable on any Thread. (It has same meaning as in Java.)
    - atomicity (A)
        - Operation is either done or not as a whole.
    - serializability (S)
        - Operations are serialized in some order (they cannot disappear).
        - This is a new property not mentioned in other memory models, since
          Java and C++ do not have dynamically defined fields.
        - Scope of the serialized operations is its container, e.g. an object is
          a serializability scope for instance variables.

- a memory model describes the interactions of threads through memory and their
  shared use of the data
- defines
    1. atomicity
    1. volatility
    1. serializablitiy
    1. synchronization barriers
    1. visibility
- java has had its current memory model since 2004 (it had older one)
- C and C++ have a formal memory since 2011
- ruby does not have any documented memory model
    - the MRI GIL provides an implied memory model but no garuantees
- https://docs.google.com/document/d/1pVzU8w_QF44YzUCCab990Q_WZOdhpKolCIHaiXG-sPw/edit#heading=h.gh0cw4u6nbi5
  is an attempt to start one
