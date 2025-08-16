http://www.erlang.org/doc/tutorial/introduction.html

Erlang has a bunch of interop methods

1. ports
    - spawn an erlang process that manages communication (over a pipe or
      similar) with another OS process
    - -- slower than other options
    - ++ much less dangerous than other options
2. Linked in driver
    - they are a variation on ports
    - compile a C module and dynamically link it into the Erlang VM
    - ++ fast because no context switches between erlang VM and C
    - -- dangerous. If the C code crashes it will take down the erlang VM
3. distributed erlang
    - node to node communiation
    - nodes can be all erlang or a mixture of erlang and any lang that supports
      the distributed erlang interface e.g. C, Java
    - there is an `erl_interface` C lib to help write C code that interops with
      Erlang
4. over TCP/UDP

## Native implemented functions

- simpler more efficient than ports way of calling C functions
- introduced in Erlang R13
- most suitable for synchronous C functions that don't ahve side effects
- C functions appear like erlang functions in modules just like any other
  function

http://www.erlang.org/doc/tutorial/nif.html

1. compile your C code into a shared library
2. dynamically link the shared lib into the erlang VM

Pros/cons of NIFs

- -- if the NIF crashes it will bring the Erlang VM down
- ++ fastest way to interface with C code (no context switch)
