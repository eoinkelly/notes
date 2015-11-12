## Disassembly of BEAM code

BEAM has 158 instructions

The erlang compiler can generate

1. byte code (.beam files)
2. erlang assembly (.S files)

When a `.beam` file is loaded into the VM it is transformed:

* some instructions are merged together into "super instructions" for efficiency
* some instructions are replaced e.g. addition replaced with increment

It is this "transformed representation" that is run. You can peak at the transformed representation by "disassembling" the loaded module.

```
elixirc simple.ex
# creates Elixir.Simple.beam

# peek at the file
objdump -C Elixix.Simple.beam

ERL_COMPILER_OPTIONS="'S'" elixirc simple.ex
# creates simple.ex.S

%% compile an elixir file
iex> c("simple.ex")

%% disassemble it
iex> :erts_debug.df(Simple)
# creates Elixir.Simple.dis
```

## .dis files

* created by `:erts_debug.df`
* disassembled BEAM code
* represents the code that is actually run (after BEAM optimizer has run)

The format of each line is:

```
memory_address: instruction_operand_types arg0 arg1 ...
```

Operand types include

* I = integer
* a = atom

etc.

Move instructions in disassembly seem to be of the form `move src dst`

Things I noticed in the disassembly

* module names and
    * `defmodule Simple` becomes `Elixir.simple`
    * are atoms even if they have `.` e.g. `'Elixir.Simple'`
* each module gets its own implementation of `module_info/1` that wraps around
  the built-in erlang `erlang:get_module_info/2`
* function name are atoms

## BEAM architecture

* BEAM has virtual registers like a real CPU would
* BEAM is a register based machine - other examples
    * Android dalvic
    * Parrot
    * Lua
* JAV (Joes abstract machine) was a stack based machine
    * other examples of stack based VMs
        * JVM
        * Forth
    * a stack based machine has no registers

Compared to a Register VM, a Stack based VM is:

* ++ simpler to implement than register based VM
* ++ more compact bytecode
+ ++ minimal CPU state
* -- potentially increased memory access (you move a lot of stuff around)
* -- no virtual registers to map to real ones (potentially uses underlying
  hardware less efficiently)

Register based VM can get really good results from "threading" bytecode (not
the concurrent kind of threading) - it is a method of dispatching the next
instruction from the previous.

## BEAM registers

| Register       | Purpose                     | Appears in .dis code as
=========================================================================
| R0 - R255      | general purpose             | x(n)
| FR0 - FR15     | floating point operations   | fr(n)
| tmpA, tmpB     | temporoary registers        | not visible in code
| stack slots    | local variables             | y(n)

The BEAM is strongly typed - it has a bunch of instructions for type checking:

* is_integer
* is_tuple
* etc.

## Elixir on the BEAM

* Elixir compiles into BEAM byte code (via Erlang Abstract Format).
All Elixir modules start with the Elixir. prefix followed by the regular Elixir name.

To invoke an elixir module from erlang

```erlang

'Elixir.SomeModule':some_func(Arg1).
```

* Elixir is structured similar to Erlang’s OTP.
* It is divided into applications that are placed inside the lib directory


## Erlang compliler

* If you pass `debug_info` to the rlang compliler it will include that debug
  info in the form of "abstract" code.
* Erlang debugging tools like
    * Debugger
    * Xref
    * Cover
    require this info to be present.
* Debugging info can also be encrypted (presumably for distribution of
  debuggable builds to clients.

You can pass
* output of parser step via `'P'` option
* output after *all* sources transfromations via `'E'` option
* output assembler code via `'S'` option


> Elixir also brings a new underlying AST to the table, instead of the Erlang AST where everything form has its own representation, the Elixir AST has a far more uniform representation, which makes meta-programming far easier.

> variables can be re-bound in sequences. This is actually ok, the resulting forms can still be normalized into a static-single-assignment (SSA) form.
