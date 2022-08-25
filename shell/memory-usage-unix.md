# Unix memory usage

Sources

* https://shopify.engineering/ruby-execution-models
https://www.baeldung.com/linux/process-memory-management


Overview

* Memory is counted in pages
* Memory is reported in either pages or KB (depending on tool)
* Memory stats come from inspecting `/proc` on linux
* Resources
    * http://elinux.org/Runtime_Memory_Measurement
* Threads share the same address space so have the same values of RSS, VSZ etc.

Q: how do I see PSS? seems like smem is best tool for it but not a standard install

## Types of memory

The address space of a process is usually the same size as physical RAM (TODO: check)
Not all addresses in the address space are mapped into a memory area
Memory areas are contiguous chunks of address space that the process can actually access
If the process tries to access an address outside a memory area then the kernel kills it iwth a SEG fault

Memory areas can be

* a file from disk mapped in
    * reads from that memory area are turned into reads from the file on disk by the kernel
    * Q: how many actual physical ram pages are used? diff if file is read-only vs writeable?
* a collection of physical pages of RAM
* measure memory areas:
    ```bash
    pmap -x PID # shows mem areas and RSS for each area
    pmap -X PID # shows mem areas, RSS, PSS etc. (on available on older kernels)
    ```


* Actual physical RAM pages
* Shared
    * a page of physical memory that is mapped into this process address space but **may** also be mapped into other processes. This leads to two cases:
        1. the memory is truly shared between N processes.
            * RSS will count these pages as being "within" this process so overcount memory usage
            * PSS splits these pages evenly between all users when counting
        2. a process can load a shared lib that no other process on the system needs so technically it is shared but really it should be counted as a requirement for this process.
        * This memory would presumably be freed once this process ends?
        * Is this memory counted in USS?
    * some pages are shared over the full lifetime of a process (if they are read-only)
    * some pages will become unshared if the process writes to them (CoW)
* Private
    * physical memory pages mapped into just this process' address space
*

## Measurements of memory

* VSZ Virtual Set Size
    * Total amount of memory a process may hypothetically access
    * When a process is started, VSZ becomes RSS
    * This size also includes memory that may not reside in RAM, such as memory where data has not yet been written after memory is allocated using malloc.
    * VSS is of little use for determining the actual memory usage of a process.
    * can be measured with
        * `ps aux` (output is in kB)
        * `top` (output in kiB)
    * Q: does VSS include files on disk mapped into the process address space?
* RSS
    * how much memory allocated to a process during it's execution
    * it counts all shared memory pages as part of the processes allocation so isn't a good metric for true memory usage
        * shared pages can be
            * OS libs
            * pages shared from a parent process e.g. puma master sharing pages with puma workers
* PSS
    * like RSS but shared pages are evenly distributed across the processes which share them
    * you can sum the PSS values of a bunch of processes together and get a good idea of actual memory usage
    * measuring
        * `/proc/$PID/smaps`
        * `/proc/$PID/smaps_rollup` (if on modern kernel)
        * `smem -kt $PID`
* USS Unique Set Size
    * might be a term created by `smem`???
    * To verify: USS is not direclty reported by the kernel, it is calculated by smem
    * represents the private memory of a process. In other words, it shows libraries and pages allocated only to this process
    * shows you the actual cost of launching another instance of this process
    * shows you how much memory you would get back from killing this process
    * measuring
        * `smem -kt $PID` is the only thing I know of
        * if it's not in /proc/pid/smaps_rollup does that mean it's a calculated value?

For a single process, generally speaking, the order of memory footprint is as follows: VSS >= RSS >= PSS >= USS

What exactly does top virt,res,shr measure?

## Complications

* some system calls use copy-on-write for shared memory pages so these may go
  from shared to not-shared
* mmap won't copy a full file into memory when you memory map it - it will only
  load the pages the app acutally uses - this means that while a 1GB file might
  actually be mapped into the virtual memory space it will only be using a
  fraction of that RAM

## Get page size

To get page size on a system:

```
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

* ps
* top
* free (linux only)
* smem https://linux.die.net/man/8/smem

### Tool: smem

* you can run filter it to a user or a process (handy when you use -t to get totals)

```bash
root@1fc95720de18:/# smem -kt
  PID User     Command                         Swap      USS      PSS      RSS
    1 root     bash                               0     1.7M     2.3M     2.9M
 1174 root     /usr/bin/python3 /usr/bin/s        0     9.5M    10.1M    10.7M
-------------------------------------------------------------------------------
    2 1                                           0    11.2M    12.3M    13.5M
```

### Tool: ps

```
# ps aux | grep ruby # on a mac
USER              PID  %CPU %MEM      VSZ    RSS   TT  STAT STARTED      TIME COMMAND
eoinkelly       98243   0.0  1.1  2542124  90972 s006  S+    8:36am   0:03.66 /Users/eoinkelly/.rbenv/versions/2.2.2/bin/ruby bin/rails s
eoinkelly        2469  23.4  0.1  2465544   7008 s004  R+    8:39am   0:03.69 ruby forever.rb
```

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


### Tool: vmmap and pmap (linux)

```
# mac
sudo vmmap -resident <PID>

# linux
i think equivalent is pmap ???
```

