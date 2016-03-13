# GDB

* gdb has a "cli mode" and a "tui mode"

```
gdb ./a.out

gdb> start
```

## TUI mode

ctrl-x a will enter "text user interface mode" which is a bit prettier
ctrl-x 1 - show source
ctrl-x 2 - open a second window and cycle through assembly, registers, ???

## Python scripting

GDB it has a python interpreter built in and provides itself as an API to
python so you can script breakpoints, pretty print structures etc. in python

```
(gdb) python print('hello')
hello
(gdb)
```
