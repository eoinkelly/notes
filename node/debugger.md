
# Node debugger

* the debugger is implemented by V8
* https://nodejs.org/api/debugger.html
* the debugger talks ot the running node process via TCP port 5858 (default)
* it has a function API and a message API (the latter can be used out of process)
* I guess `node debug foo.js` uses the function API
* I guess `node --debug foo.js` listens on tcp:5858 for the message API
* Protocol: https://code.google.com/p/v8-wiki/wiki/DebuggerProtocol

Commands

1. help
1. run (r)
    * start the script (happens automatically at start)
1. cont (c)
1. next (n)
    * step over the current statement
1. step (s)
    * step into the current statement
1. out (o)
    * step out of current stack frame (or context - which is it???)
1. backtrace (bt)
1. setBreakpoint (sb)
1. clearBreakpoint (cb)
1. watch
1. unwatch
1. watchers
1. repl
    * starts a repl
    * no help available in it
1. restart
1. kill
1. list(n)
    * show n lines of context
1. scripts
    * TODO: don't know what this does
1. breakOnException
1. breakpoints
1. version

* `print` does not exist anymore - use `repl` instead



## The repl

* The repl command allows you to evaluate code remotely.
* There don't seem to be any commands that work in the repl

# Other ways of debugging

To debug a node process from outside it

1. put the process in "debug mode" by
    * send SIGUSR1 to it if it is already running
    * start with with the `--debug` command line flag
        * it makes the process listen on 5858 for debug client
        * it does not automatically stop the process
    * start with with the `--debug-brk` command line flag
        * this does same as `--debug` but it also stops on the first line
2. connect to it
    * `node debug -p <pid>`
    * `node debug <URI>` where URI can be `localhost:5858` by default

```sh
# shell 1
node --debug foo.js

# or

node foo.js
ps aux | grep foo
# get pid
kill -s USR1 <PID>

# or

# run in debug mode, break on first line
node --debug-brk foo.js

# shell 2
node debug localhost:5858
```
