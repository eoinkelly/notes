# Lua in Lightroom Classic

Source: The PDF doc in the SDK

- `Info.lua` is a manifest file, not intended as a "script" per se
- type of plugin is determined by folder and file naming
- lightroom 5 uses lua 5.1.4
- Lightroom defines the `import` function as a way of associating a "namespace"
  with a local variable
    - `import` returns a constructor function not a table!
        - some namespacess return both a constructor function as a table that
          you can also call things directly on e.g. LrFtp
- parens around function args can be skipped if you are passing a single string
  or table

## Namespaces

- LR does not use or support the module system introduced in lua 5.1
- In LR a _namespace_ is a table with a set of functions as values
- LR provides an OO (classes and instances) API for use in lua
- LR provides namespaces and classes - plugins cannot define either namespaces
  or classes
    - Q: how is that enforced?
- A subset of the normal lua namespaces and global functions are available
- use the `import` function to assign a namespace to a local variable

Some namespaces are also classes (using the lua "invoke table as constructor"
trick). This means that

- you can call them as a constructor function
- call functions directly on them (akin to "static methods")
- examples of namespaces which are also classes:
    - `LrLogger`
    - `LrColor`
    - `LrFtp`
    - `LrFunctionContext`
    - `LrProgressScope`
    - `LrRecursionGuard`
    - `LrView`
    - `LrXml`

Usually `import` returns a constructor function and you then create an instance
of the object to work with. There are some exceptions e.g. the "active catalog"
is an object you retrieve a reference to but you don't instantiate yourself

```lua
local LrLogger = import("LrLogger") -- import returns a constructor function not a table!
local logger = LrLogger("myPlugin") -- create an instance of the logger

-- call methods on the logger object
logger:enable('print')
logger:warn('bad thing')
```

global lua functions available:

    assert(), dofile(), error(), getmetatable(), ipairs(), load(), loadfile(),
    loadstring(), next(), pairs(), pcall(), rawequal(), rawget(), rawset(),
    select(), setmetatable(), tonumber(), tostring(), type(), unpack()

namespaces fully available

    io, math, string

namespaces partially available:

    os
    table
    coroutine
    debug

## LR require() is custom

- similar to normal lua `require` but
    - files must be in same plugin

## Logging and debugging

Lightroom SDK provides a logger. View logs in Console app built into macOS.

You can focus on just your log messages by

1. Filter for the `Adobe Lightroom Classic` process (visually by right-clicking
   on an instance of it) 2. then also filter by some unique string you add to
   your log files e.g. `MYLOG`

```lua
local LrLogger = import 'LrLogger'

-- Create the logger and enable the print function.
local myLogger = LrLogger('exportLogger')
myLogger:enable("print") -- Pass either a string or a table of actions.

local function outputToLog( message )
	myLogger:trace(message)
end

outputToLog("MYLOG: some message")
```
