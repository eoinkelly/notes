# Unix memory usage

* Memory is counted in pages
* Memory is reported in either pages or KB (depending on tool)
* Memory stats come from inspecting `/proc` on linux
* Resources
    * http://elinux.org/Runtime_Memory_Measurement
* Threads share the same address space so have the same values of RSS, VSZ etc.

## Complications

* some system calls use copy-on-write for shared memory pages so these may go
  from shared to not-shared
* mmap won't copy a full file into memory when you memory map it - it will only
  load the pages the app acutally uses - this means that while a 1GB file might
  actually be mapped into the virtual memory space it will only be using a
  fraction of that RAM

## Get page size

To get page size on a system:

````
#mac
??

#linux
??
```

* It is difficult to get an accurate count of pages used by a process.
    * some tools double-count pages that are shared between processes
    * some tools don't account for whether pages are in RAM or swap

## Tools

THe common tools are:

* Activity monitor (Mac only)
* ps
* top
* free (linux only)
* vmmap (Mac) and pmap (linux)

### Activity monitor (Mac only)

Good resource on how memory works on MacOS: http://stackoverflow.com/a/1954774/356025

What is mac memory compression?

In activity monitor:

* Mac Private memory RPRVT
    * stack memory + malloc()'d memory (it may be shared with a forked child as
      long as copy-on-write has not been triggered on it by a write from either
      parent or child)
* Mac Shared memory RSHRD
    * memory that is currently visible in the address space of multiple processes
    * and memory that is likely to be shared e.g. the TEXT segment (binary code
      of the app)
* Mac Real memory RSIZE
    * Amount of RAM currently assigned to the process (incl. private and shared)
    * can be bigger than private+shared if process has more mem assigned to it
      than it is currently using (kernel can quickly take it back if it needs
      to)
    * can be smaller than private+shared if process has requested and received
      virtual memory but it has not yet used it so kernel has not put those
      pages into RAM
    * Seems to be roughly RSS in linux world

Wired memory = memory that must never be paged to disk e.g. kernel stuff

### ps

```
# ps aux | grep ruby # on a mac
USER              PID  %CPU %MEM      VSZ    RSS   TT  STAT STARTED      TIME COMMAND
eoinkelly       98243   0.0  1.1  2542124  90972 s006  S+    8:36am   0:03.66 /Users/eoinkelly/.rbenv/versions/2.2.2/bin/ruby bin/rails s
eoinkelly        2469  23.4  0.1  2465544   7008 s004  R+    8:39am   0:03.69 ruby forever.rb
```
# ps output

ps tends to report memory usage of a process as if it was the only process running on the machine

TT = Controlling terminal (if any)
    * s004 means the controlling terminal for this process is /dev/ttys004
    * con means the controlling terminal is the console
    * - suffix means this process can no longer reach that controlling terminal
* USER = name of user
* PID = process ID
* %CPU = ??
* %MEM = % of memory used by the process
    * which memory? it's vmem max or real physical mem?
* VSZ (Virtual Set Size)
    * Reported in KB
    * All memory that a process can access
    * Includes all memory even if it is swapped out
    * Includes all memory (resident and swapped) from shared libraries
* RSS (Resident Set Size)
    * Reported in KB
    * The amount of physical RAM being used by the process
    * Includes
        * All the stack
        * All the Text (code) pages that have actually been loaded (these are
          shared between all running instances of the process)
        * All the heap that isn't swapped out
        * Shared library pages that are actually in RAM (these can be shared by
          many processes on the machine)
    * Does not include
        * Any memory that has been swapped out to disk
    * In summary: RSS is all the memory for this process that is currently in
      RAM but it does not show you how much of that memory is shared with other
      running processes


## vmmap and pmap (linux)


```
# mac
sudo vmmap -resident <PID>

# linux
i think equivalent is pmap ???
```

