# GDB

- gdb has a "cli mode" and a "tui mode"

```shell
$ gdb ./a.out # start in default mode
$ gdb -tui ./a.out # start in text user interface mode
gdb some_prog 1234 # debug a pid

gdb --args some_prog -arg1 -arg2
gdb -silent # run without all the front-end stuff
```

```gdb
start

print
explore
x
list

info functions

info # help on info commands
i # short for info
info locals
info registers
info all-registers # includes d* registers too - waht are these?

show # shows state of gdb itself, not your program
layout asm

https://sourceware.org/gdb/onlinedocs/gdb/TUI-Commands.html#TUI-Commands

tui enable
dui disable


# you can scroll windows

info win # tells you which window is active
focus next|prev|src|asm|regs|command # change focus

use up/down arrow to scroll

run # run the program

# set breakpoint
break name_of_func

next
n # next (step over)
step
s # step into

backtrace
bt

print
p

continue
c

quit
q
ctrl-d

shell <CMD>
!<CMD>

update # resets the screen (required after shell output sometimes)
ctrl-l # clear screen

set logging on
set logging off
set logging file <FILENAME>
show logging # show logging settings

<blank line means run last command again>
    * except for some commands e.g. run
```

## TUI mode

ctrl-x a will enter "text user interface mode" which is a bit prettier ctrl-x
1 - show source ctrl-x 2 - open a second window and cycle through assembly,
registers, ???

## Python scripting

GDB it has a python interpreter built in and provides itself as an API to python
so you can script breakpoints, pretty print structures etc. in python

```
(gdb) python print('hello')
hello
(gdb)
```
