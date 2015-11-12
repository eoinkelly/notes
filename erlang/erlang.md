# Erlang

Erlang has two paradigms

1. Functional
2. Concurrent

Sources:

* http://learnyousomeerlang.com

* *Most* functions in erlang are referentially transparent
    * There are some exceptions e.g.`today()`
    * Erlang sticks to functional programming principles until it is not practical to do so.
* uses the Actor model
    * each actor is a process in a virtual machine
    * an actor has an inbox
    * it waits for a message in the inbox, does something and sends a response
    * actors ignore messages they do not understand
* let it crash
    * don't check for errors everywhere, just crash if something goes wrong and let your supervisor handle it
* man pages
    * stored in `/usr/local/opt/erlang/lib/erlang/man`
    * `erl -man lists` or add that dir to MANPATH

> keep in mind that randomly throwing parallelism at a problem is not enough to
> make it go fast

> Erlang is also said to be able to scale in a directly proportional manner to
> how many cores your computer has, but this is usually not true: Bad Graph:
> Speed vs Cores: It just scales! it is possible, but most problems do not
> behave in a way that lets you just run everything at the same time

```erlang
%% built-in line editor uses a subset of emacs ^A start of line, ^E end of line etc.
help(). %% show help
q(). %% quit
init:stop(). %% quit
^G then q %% quit
^G then h %% see list of options for managing jobs, remote shells, quitting
f(). %% clear all variables (only works in shell)
Foo = 12. %% bind variable name Foo to value 12
f(Foo). %% clear Foo binding (only works in shell)
```

* variables
    * names begin with uppercase
    * can only be assigned once
    * = will try to match its left and right operands
    * names begining with `_` or just `_` are never bound by erlang - they are ignored.
        * `_` is not just a naming convention!
        * _ is always seen as unbound and can be used as a wildcard in pattern matching.
* atoms
    * names start with lowercase
    * contants whose name is their value
    * atoms are kept in a global atom table and never GCed
        * this means you can memory bloat or overflow the table
        * WARNING: never create atoms dynamically!
    * there are a small selection of atoms that are reserved words:
        1. after
        2. and
            * always evaluates both operands (cannot be used as short circuit)
        3. andalso
            * only evaluates RHS argument if LHS one evaluates to true
            * is the shortcut version of `and`
        4. begin
        5. band - binary and
        6. bnot - binary not
        7. bor - binary or
        8. bsl - binary shift left
        9. bsr - binary shift right
       10. bxor - binary xor
       11. case
       12. catch
       13. cond
       14. div
       15. end
       16. fun
       17. if
       18. let
       19. not
       20. of
       21. or
            * always evaluates both operands (cannot be used as short circuit)
       22. orelse
            * only evaluates RHS argument if LHS one evaluates to false
            * is the shortcut version of `or`
       23. query
       24. receive
       25. rem
       26. try
       27. when
       28. xor
    * bare words e.g. `foo`, `name` or 'enclosed in single quotes'
        * can have spaces and punctuation if enclosed in single quotes
    * bit like Symbols in ruby
    * true and false are just atoms - there is no "truthy" and "falsy" values
    ```erlang
    1 == true # nope, there is no "truthiness" in erlang
    0 == false # nope, there is not "falsiness" in erlang
    ```
* equality operators
    * `=:=` is `==` in other langs
    * `=/=` is `!=` in other langs
    * erlang has strict type checking for most operations but will let you
      _compare_ things of different types (for pragmatic reasons)
    * you can compare anything with anything in erlang!
    * the defined order is
    ```
    number < atom < reference < fun < port < pid < tuple < list < bit string
    ```

    ```erlang
    a =:= b # strict equality
    a =/= b # strict inequality

    # these operators will convert floats and integers
    a == b # equality with type coercion
    a /= b # inequality with type coercion

    5 =:= 5.0. # false
    5 == 5.0. # true

    a > b
    a < b
    a >= b
    # a <= b # WRONG!
    a =< b # notice that it is written "equal or less than" in erlang
    ```

### tuples

* tuples use curly braces
* a two tuple where the first element is an atom is called a "tagged tuple"
    * tagged tuples are a quick way to make a new type
    * erlang is compiled so tuples basically become a global table of ints at
      runtime

```
%% tagged tuples
Temp = {celsius, 23.45}.
OtherTemp = {kelvin, 444}.

X = 12.
Y = 44.
Point = {X, Y}.
{A, B} = Point.
A. %% 12
B. %% 44
{C, _}F
C. %%  12
```

### lists

* are hetrogenous
* erlang strings
    * are just lists - there is no "real string" in erlang
    * erlang will print a list of numbers as a string if the numbers are printable
    * uses ~ to denote _tokens_ in strings
    * tokens behave a bit like printf placeholders and special char escapes in
      other langs. `io:format("Value is ~p and title is ~s~n", [Value, Title]).`
        * ~s = replace with a string
        * ~n = replace with newline
        * ~p = replace with a pretty printed erlang value
    * erlang has no built-in string type for historical reasons
    * this is improving over time - the VM now supports unicode
* list operators
    * ++ concatenates lists
    * -- remove from LHS any element that is a member of RHS
        * if LHS has 33 twice and RHS has 33 then it will only remove 33 once
          from LHS
    * hd/1 returns the value which is head of list
    * tl/1 returns the tail of the list (itself a list)
    * | (cons operator)
        * used to build list by concatenating LHS value onto RHS list
        * used to split lists in pattern matching
        * `[1 | 2]` gives an "improper" list
            * can be used for pattern matching
            * cannot be provided to any built-in functions
            * a proper version of this list would be `[1 | [2 | []]]`
            * when you write `[2]` erlang automatically makes it "proper" for you

```erlang
%% strings are lists in erlang
X = [97,98,99].
"abc"

Y = [1, 44.5, foo, {a, b}, 4, 55].


A = [1,2,3,4].
B = [4,3,5].

A -- B.
[1,2]

C = [2,5,1].
[2,5,1]

A -- C.
[3,4]

AA = [1,1,2,2,3,3].
[1,1,2,2,3,3]

AA -- C.
[1,2,3,3]


%% These are equivalent
[a, b, c, d] %% erlang makes this "proper" for you
[a | [b | [c | [d | []]]]] %% manually making a "proper" list
```

#### List comprehensions

* A succinct way of turning 1+ Lists into a new List

`Pattern <- List` is a `Generation expression`

```
NewList = [Expression || Pattern1 <- List1, Pattern2 <- List2, ..., PatternN <- ListN, Condition1, Condition2, ... ConditionN]
```

* You can use pattern matching in the Pattern to filter the list (so a
  Generator expression does not have to be the exact same as its List)

```erlang
Weather = [{toronto, rain}, {montreal, storms}, {london, fog}, {paris, sun}, {boston, fog}, {vancouver, snow}].
FoggyPlaces = [X || {X, fog} <- Weather].
```

#### Aside: BIFs

* Built-in functions
* Functions provided by the VM that are written in same lang VM is implemented
  in itself for speed (today this is C but was prolog historically)
* Examples: length(List)

### Bit syntax

* erlang has good bit/byte manipulation for historical reasons
* erlang is not as fast at C/C++ for number crunching but is very convenient
* makes it easy to pack other data types into a binary representation
* you can pattern match on binaries
    * makes it easy to unpack binary data into other data types

* a binary is 1+ segments separated by commas delimited by `<<` and `>>`

    <<segment1, segment2, ... segmentN>>

A segment can be specified in 4 forms

```
1. Value
2. Value/TypeSpecifierList
3. Value:Size
4. Value:Size/TypeSpecifierList
```

where

* Value
    * the actual data to be packed/unpacked
    * can also be a variable reference
* Size
    * a multiplier for the no. of "units" in this value
    * see below for `Unit`
* TypeSpecifierList is a `-` separated list of 1+ of the following
    1. Type
        * integer|float|binary|bytes|bitstring|bits|utf8|utf16|utf32
        * `bytes` is an alias for `binary`
        * `bits` is an alias for `bitstring`
    1. Signedness
        * signed|unsigned
    1. Endianness
        * big|little|native
        * only matters when type is one of integer, utf16, utf32, or float
    1. Unit
        * the size of the segment in bits
        * `unit:X` where 1 <= X <= 256
        * Size * Unit = the no. of bits the value will take up
        * unit is automatically set for
            * utf8
            * utf16
            * utf32
        * is usually used to ensure a particular byte alignment

The above "packing and unpacking specification" provides a DSL for converting other data types to/from a binary represtation

```erlang
Red = 16#FF0000. %% hex literal
RedPixel = <<Red:24>>. %%  <<255,0,0>>
RawPixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>.

<<Pix1:24, Pix2:24, Pix3:24, Pix4:24>> = RawPixels.
Pix1. %%  13970820

<<R:8, G:8, B:8>> = <<Pix1:24>>.
R. %%  213
G. %%  45
B. %%  132
Pix1. %%  13970820
<<Pix1:24>>. %%  <<213,45,132>>

%% notice we can do pattern matching
<<R2:8, Rest/binary>> = RawPixels.
R2. %%  213
Rest. %%  <<45,132,64,76,32,76,0,0,234,32,15>>
```

Bit strings

    <<"i am a bitstring, a bit like a C array">>
* ++ more space efficient compared with "list strings"
* -- harder to manipulate
* -- don't use them to tag tuples, atoms are more efficent for that.

Binary comprehensions

* erlang has binary comprehensions that work like list comprehensions
* slightly different syntax but same purpose: to turn binaries into other types and vice versa

```erlang
[ X || <<X>> <= <<1,2,3,4,5>>, X rem 2 == 0]. %% [2,4]

16> Pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>.
<<213,45,132,64,76,32,76,0,0,234,32,15>>
17> RGB = [ {R,G,B} || <<R:8,G:8,B:8>> <= Pixels ].
[{213,45,132},{64,76,32},{76,0,0},{234,32,15}]
18> RGB.
[{213,45,132},{64,76,32},{76,0,0},{234,32,15}]
19> NewPixels = << <<R:8, G:8, B:8>> ||  {R,G,B} <- RGB >>.
<<213,45,132,64,76,32,76,0,0,234,32,15>>
20> Pixels == NewPixels
20> .
true
21> Pixels == NewPixels.
true
```

#### Aside: how to clear the erlang shell:

https://gist.github.com/hamidreza-s/10520105

```erlang
%% move cursor to beginning of the line
io:format("\e[H").

%% clear the console
io:format("\e[J").

%% both
io:format("\e[H\e[J").
```

## Modules

* a module is a collection of functions grouped in single file under a single name
* the `erlang` module
    * contains many common functions:
        * `hd/1` `length/1`
        * length/1
        boolean opertaors
    * _most_ of its functions are always automatically imported in erlang
    * access the explicitly via `erlang:length([3,4])`
* function access syntax is `module_name:func_name(args...)`
* Modules have _attributes_ at the start of the file
* Attributes setup metadata about the module
* Module name is the only required piece of that metadata
    * module name is an atom (hence is always lowercase)

Module Attributes

* -module(modname).
    * takes a atom representing module name
    * filename should be `modname.erl`
* -export([func/arity, func/arity, ... ]).
    * takes a list
    * list is of function/arity
    * ??? what type is that ???
* -behaviour( ???
* -import(module, [func/arity, func/arity ...]).
    * is just a shortcut for writing code
    * many erlang devs believe using import hurts readability
* -define(MACRO, expression).
    * Erlang macros are pretty close to C #define
    * simple expresstions that will be text replaced by compiler before
        compilation
    * useful for constants etc.
    * used as `?MACRO` inside the file
    * macros are module attributes so are presumably? scoped to a single file
* -compile([flag1, flag2, ... flagN]).
    * an alternative to defining compiler flags on command line or in shell
* -vsn('some unique vsn probably a terrible idea').
    * specify the unique fingerprint used in hot code loading
    * not sure about uses for this

```erlang
%% The form is: -AttributeName(AttributeValue)

%% module name is an atom
-module(eoinsmodule).
```

There seems to be a pattern of modules exporting some helper functions that
explain what they are about e.g. `hipe:help().`

## Module introspection

Modules automatically define `module_info/0` and `module_info/1` which dump the
metadata about the module e.g.

```erlang
adder:module_info().

[{module,adder},
 {exports,[{add,2},
           {hello,0},
           {greet_and_add_magic,1},
           {module_info,0},
           {module_info,1}]},
 {attributes,[{vsn,[162663618642573659805142038623569107523]}]},
 {compile,[{options,[native]},
           {version,"6.0.1"},
           {time,{2015,10,28,18,17,57}},
           {source,"/Users/eoinkelly/Dropbox/Eoin/Notes/on-github/erlang/adder.erl"}]},
 {native,true},
 {md5,<<79,213,217,205,26,221,80,140,114,89,102,195,227,76,24,46>>}]

%% can also filter by the first atom in each tuple
adder:module_info(exports).
```

You can also use this in elixir

```elixir
List.module_info()
List.module_info(exports)
```

#### VSN

* An automatically generated unique fingerprint for each version your code (excluding comments)
* used in hot code loading
* you can specify it as a module attribute

### Functions

* Function clause
    * general form is
    ```
    name(args) -> body
    ```
* Function declaration
    * A function declaration is a ; separeated collection of function clauses
      that ends with a .
    * counts as one statement so ends in `.`

```erlang
-module(greeter).
-export([greet/2]).

greet(irish, Name) ->
  io:format("Cead mile failte ~s~n", [Name]);
greet(kiwi, Name) ->
  io:format("Kia ora ~s~n", [Name]);
greet(_, Name) ->
  io:format("Hello there ~s~n", [Name]).
```

* function name must be an atom
* function body is one or more erlang expressions separated by commas
* erlang has no return keyword, it uses implicit returns
* last expression in the function ends in .
* erlang functions _always_ return a value (like ruby)


Aside: function clauses vs a big case statement in othe langs

If function clauses are good then why don't I wrap the contents of my ruby methods ina  big case statement?

* in ruby we would have to bind all possible parameters. Function clause mean only the params you care about are bound - this makes the function bodies simpler.
* You can check on type of args in both cases
* the syntax is nicer in erlang/haskell etc.
* erlang pattern matching means that a lot of breaking down of args can happen in the function signature - ruby would have to do that manually


Pattern matching

* erlang function signature pattern matching is advanced
* Can bind variables in it

```erlang
valid_date_2(Date = {Y, M, D}, Time = {H, Min, S}) ->
  %% stuff
```

Guards

* pattern matching can
    * specify a particular number of args
    * break down compound types (tuples, lists) into their scalar parts
    * filter compound types (tuples, lists)
* pattern matching cannot
    * say anything about the value of a particular type

Guard expression

* is added to the fuction clause signature
* must retrun true for erlang to consider this clause a match for the args it has
* guard fails if it returns false or throws exception
* functions you can use in guards
    * there is a subset of built-in functions you can use
        * comparison and boolean evaluation
        * type checking
        * math operations
    * guards cannot accept user defined functions because guards need to be
      pure functions - erlang has no way to verify that your guard function
      would be pure so it disallows it.
* guards can be separeated by , or ;
    * `,` acts like `andalso` except
        * `andalso` can be nested in a guard but `,` cannot
        * `OP1 orelse OP2` will always fail if OP1 throws an exception
    * `;` acts like `orelse`
        * `orelse` can be nested in a guard but `;` cannot
        * `OP1 ; OP2` will not fail if OP1 throws an exception but OP2 passes

```
name(arg, arg ... arg) when guard sep guard sep ... sep guard ->
where sep is , or ;
```

### if

erlang if statements

* should probably be named something else because they don't function like other lang ifs
* return a value
* behaves a lot like a guard clause
    * `;` and `,` work the same as in guard clause
* have many "branches"
* sometimes have a "true branch" aka else statement
    * erlang programmers are encouraged not to use "true branches" but rather
      to cover all the logical possibilities with other if statements because
      it makes code easier to read.

```erlang
basic_if(A, B) ->
  %% note the ; at end of each branch except the last
  RetVal = if A =:= B -> io:format("are equal~n");
              A > B -> io:format("first is bigger~n");
              true -> io:format("fell into true branch~n")
           end,
  %% A contrived example to show that if expressions return a value
  {all_well, RetVal}.

other_if(A, B) ->
  %% a contrived and broken example to show usage of , and ; in else clause
  if A =:= B, A > 5; B < 20 -> io:format("are big and equal~n");
     true -> io:format("NOT big and equal~n")
  end.
```

### case of

* a `case...of` expression is basically the same as a bunch of function heads
  with guards.
    * each branch of the case statment could be converted into a function head
    * the right one to use depends on context
    * sometimes a case statement is a good alternative to having a function
      destructure some args and then call a multi-head private function which
      destructures w. guards
* can take a variable and run pattern matching on it and then run guards as well

```erlang
case THING of
  DESTRUCTURED_THING when GUARDS -> do_thing;
  TEST_RESULT_2 -> do_thing;
  TEST_RESULT_3 -> do_thing;
end
```

When to use case...of, multiple function heads

* both work in a very similar way
* both are represented in a very similar way in BEAM level so both have very
  similar performance characteristics
* it is not super well defined when to use each

Why use if when case...of is can do the job?

if was added to the language as a shorthand when you want guards but don't need pattern matching



## Compiling

BEAM = Bogdan/Björn's Erlang Abstract Machine

1. via erlang shell
2. via `erlc` command

Code can be compiled to be

1. a cross-platform BEAM file
2. a "native" beam file via HiPE (High Performance Erlang) project
    * contains some native and cross-platform parts
    * runs faster
    * not available on all platforms (seems ok on OSX)
    * `hipe:help().` for info

common compile flags
* debug_info
    * add debug info
* export_all
    * export all functions
* {d, Macro}
    * define Macro as true
* {d, Macro, Value}
    * define Macro as Value
* {outdir, Dir}
    * tell compiler not to compile into current dir

```erlang
%% in erlang shell

cd("/path/to/erlang/files").
c(things).
c(things.erl). %% also works
things:somefunc().
things:otherfunc().

%% You can pass compiler flags
c(things, [debug_info, export_all]).


%% generate native beam with hipe
hipe:c(things).

% use hipe compiler by passing the native atom to c/2
c(things, [native]).
```

### Types

* atom
* list
* binary
* term
* float
* tuple
* integer
* iolist
* pid
* port
* ref

Erlang is strongly typed => no automatic type coercion. You can do explicit type conversions via a family of `<type>_to_<type>/1` functions e.g. `erlang:atom_to_binary/1`

There are a bunch of `is_<type>/1` functions you can use to check type in guards

Erlang does _not_ have the equivalent of `foo.class` in ruby i.e. there is no way to introspect the type of a value. The stated reasoning for this is that erlang is about programming for the happy path and "let it crash".
Erlang has no null/nil value


## Error handling

Types of error

1. compile-time error
    * errors that can be found by the compiler
1. logical error
    * hardest to debug
    * they don't cause your program to crash
    * it just does not do what it is supposed to
    * testing is your only friend here
1. run-time error
    * exceptions
1. generated error


There are three kinds of exception in Erlang

1. errors
    * are a way for a function to stop its execution if it has no way to reliably continue
    ```erlang
    erlang:error(no_api_available).
    ```
    * reason
        * is an atom
        * can be any name
        * erlang has some built-in reason names (they will include a message when displayed)
            * badarith
            * badarg
            * undef
            * badfun
            * badmatch
            * if_clause
            * case_clause
            * function_clause
        * you will probably never generate an error with a built-in reason
1. exits
    * two kinds
        1. internal
            * triggered by `exit/1`
            * makes current process stop execution
        1. external
            * triggered by `exit/2`
    * they have very similar use cases to errors
    * they do not return a stack trace
    * used when it would be impractical or unnecessary to send a stacktrace to
      all listening processes
1. throws
    * does not crash the process - just changes control flow
    * provides a sort of non-local return or goto
    * used for error situations that the process can handle
    * `throw(any_atom_describing_reason).`
    * good idea to limit usage of throw to a single module - otherwise
      debugging gets very hard

## Processes

Erlang process is a function with some hidden state (mailbox)
* each process has a `pid` which is used to communicate with it


You can see whether the VM is in SMP mode or not from the status line

```
Erlang/OTP 18 [erts-7.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]
```

```
[smp:X:Y]
X = number of cores available
Y = number of schedulers

[rq:Z]
Z = number of run queues available
```

* `self/0` returns the pid of the current process (handy to use in shell)
* process mailbox
    * keeps messages in the order received
    * a message is taken out of the mailbox when it is read
    * use `flush/0` to inspect the contents of the current processes mailbox

Send messages to processes using `!`

```erlang
%% send <message> to a process mailbox (represented by pid)
<pid> ! <message>.

%% send <message> to multiple pids mailboxes
<pid1> ! <pid2> ! <pid3> ! <message>.
```

* receive blocks
* syntastically similar to `case ... of`

```erlang
receive
    Pattern1 when Guard1 ->
        Expression1;
    Pattern2 when Guard2 ->
        Expression2;
    Pattern3 when Guard3 ->
        Expression3;
    Pattern4 ->
        Expression4
end.
```


Aside: inspecting process state

sys:get_status(SomePid). %% only works if Pid is an OTP app

SASL is the error reporting daemon in OTP - `rb` is the module used to collect the reports.

`proc_lib` can be used to make a short-lived process OTP compatible

tracing is intended for debugging only, not for use in production


Process links
* `link(Pid)`
* `unlink(Pid)`
* links cannot be stacked - no matter how many times you call link/1 with a given Pid only one link will be created between the processes
* link two processes together so that
* if one crashes (from a throw, error, exit) then the other dies too due to a special message it receives from the crashing process
* this special message is not sent if the process (function) exits cleanly
* are bi-directional
* use spawn_link/1-3 to spawn a process and link it atomically
    * prevents errors from of your process dies before it is linked
    * (safer than doing it as separate steps)


Exit signals are an example of "special" kind of message called "signal"

* system process
    * like a normal process but can convert a signal into a normal message

```
process_flag(trap_exit, true).
```

### Registered names for processes

If you have a process with a supervisor that is able to restart the process if
it dies then you can't rely on the pid always being the same. Erlang solves
this by allowing you to register a name for a process

```erlang
regsiter(Pid, some_name).

registered(). %% see all registered processes

CurrentPid = whereis(some_name).
```

* the registered name is shared global state
    * many processes can modify it at same time
    * can be a source of race conditions
* only use named processes for
    * services unique to an instance of the VM
    * processes that will run for the whole lifetime of your app


