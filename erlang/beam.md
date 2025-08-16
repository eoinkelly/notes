# The BEAM

- BEAM
    - has its own scheduler. Erlang code is scheduled by that, not the OS
      scheduler
    - does preemptive scheduling
    - has "reductions" - does not allow a process to hog the scheduler
    - Run work queues
        - scheduler will "re load balance" if one queue is taking too long
    - Load balancing

## Monitoring

- GUI: Erlang `observer` module
- Command line: Erlang `c` module

### :observer.start

A GUI that shows

- system overview
- load charts
- application view
- process overview
- trace and messaging information
- process messages, backtrace, state
- remote node connection

### :c

- A command line equivalent of `:observer.start`

```
:c.regs # show registered procs for local node
:c.nregs # show registered procs for all connected nodes
:c.i # info summary of all processes (bit like a ps aux output)
:c.i(0,34,0) # info about a particular process
:c.bt(:c.pid(0, 79, 0)) # backtrace for a particular process
```

## Types

- BEAM is strongly typed
    - small set of types
- BEAM is Dynamically typed
    - Optional typing
    - @spec directive
        - does type annotations
        - BEAM does "success typing" via dialyzer
        - dialyzer will hurt your feelings ...
        - dialyzer will give you everything it knows about the types
        - dialyxir exists
    - TypEr (works in Erlang, not in Elixir yet)
        - shows type information
        - adds type annotations to source code

## Behaviours

- Generic implementation of common tasks
- Callbacks provide the specific details
- examples
    - gen_server
    - gen_event
    - gen_fsm
    - supervisor
- The "gen" stands for "generic"
- You can create your own behaviours

Proctor reckons you don't need to know the whole erlang language. He recommends
knowing enough to know what Erlang can provide and can use the things that
Elixir does not wrap yet.

## Languages that run on the Beam

- erlang
- Lisps
    - LFE Lisp flavoured erlang
    - Joxa
- Prolog
    - Prolog on Elrang
- Haskell:
    - Yhc
- Javascript
    - erlyjs
- microkanren
    - which is smaller version of minikanren
    - which is a small prolog
- Lua
    - luerl

## Misc cool stuff

- Webmachine
- Ling - Erlang on Xen
    - run erlang but not on the Beam
    - run erlang without an OS
- Erlang on "mega core" architecture (aka 1000 cores)

# How erlang code compiles

the code you write goes through three major representations, and you can look at
each of them.

1. human erlang
    - you write this
1. core erlang
    - kept in `.core` files
1. beam vm format
1. native code

human_erlang -> core_erlang -> beam_vm_format -> native_code
