# Erlang

Sources:

* http://learnyousomeerlang.com

* Most functions in erlang are referentially transparent
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

GOT TO http://learnyousomeerlang.com/starting-out-for-real

 variables
    * names begin with uppercase
    * can only be assigned once
    * = will try to match its left and right operands
    * names begining with `_` or just `_` are never bound by erlang - they are ignored.
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
        4. band
        5. begin
        6. bnot
        7. bor
        8. bsl
        9. bsr
       10. bxor
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
