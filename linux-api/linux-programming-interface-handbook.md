# Linux Programming Interface Book

## Aside: Building C files

```sh
# Works on Mac and Linux

vim foo.c
gcc foo.c -o exe_name
./exe_name

# make can build a single file without a Makefile
vim foo.c
make foo
./foo
```

## Aside: Tracing system calls

* strace on linux ~ dtruss on mac

```sh
#linux
strace /bin/date

# mac (needs root access)
sudo dtruss /bin/date

man dtruss
dtruss -p 1234
```

## Aside: Seeing which shared libs a binary depends on

* `ldd` prints shared library dependencies
    * usually invokes the "standard dynamic linker" with a TRACE flag set
    * in some cases it may invoke the program to find out so be careful!
* `objdump /bin/date | grep NEED` will do same thing
    * objdump shows info from object files
    * is safe to use on untrusted executables
* `otool -L` seems to be mac equivalent of `objdump`
    * `otool -L` shows shared lib dependencies

```sh
ldd /bin/date                   # linux
objdump /bin/date | grep NEED   # linux
otool -L /bin/date              # mac
```

## Aside: mac equivalent of libc

* `/usr/lib/libSystem.B.dylib` seems to be it

```sh
$ otool -L /bin/date
bin/date:
    /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1213.0.0)
```

## Aside: LibC on Ubuntu Vivid

```
# find the location of libc on the system
$ ldd /bin/date | grep libc

# run it as executable to see details
$ /lib/x86_64-linux-gnu/libc.so.6

GNU C Library (Ubuntu GLIBC 2.21-0ubuntu4) stable release version 2.21, by Roland McGrath et al.
Copyright (C) 2015 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.
There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.
Compiled by GNU CC version 4.9.2.
Available extensions:
    crypt add-on version 2.1 by Michael Glad and others
    GNU Libidn by Simon Josefsson
    Native POSIX Threads Library by Ulrich Drepper et al
    BIND-8.2.3-T5B
libc ABIs: UNIQUE IFUNC
For bug reporting instructions, please see:
```
## Aside: see current kernel details

```
$ uname -a
Linux ubuntu-xenial 4.4.0-78-generic #99-Ubuntu SMP Thu Apr 27 15:29:09 UTC 2017 x86_64 x86_64 x86_64 GNU/Linux
```
## Aside: man pages

Man page sections

1. executable programs and shell commands
    * most of the man pages I read come from here
2. system calls (functions provided by the kernel)
3. library calls (functions within program libraries
4. special files (e.g. under `/dev`)
5. file formats and conventions e.g. `/etc/passwd`
6. Games
7. Misc
8. System admin commands (things only root can run)
9. Kernel routies (non standard) e.g. `shutdown`

They are searched in the following order

```
"1 n l 8 3 2 3posix 3pm 3perl 5 4 9 6 7"
```

Number of man pages on MacOS Yosemite organised by section (Calcuated by:
`apropos . | grep "(9)" | wc -l`)

1. 1763
2. 160
3. 1635
4. 32
5. 221
6. 1
7. 244
8. 406
9. 5

## Preface

Standards

* circa 2001
    * POSIX.1-2001 (Portable Operating System Interface)
    * SUSv3 (Single UNIX Specification version 3)
* circa 2008
    * SUSv4 (Single UNIX Specification version 4)
    * POSIX.1-2008

## Chapter 1

* UNIX and C grew together
    * both were "designed by professional programmers for their own use"

* UNIX
    * predates C by a few years (unix 1971, unix in C: 1973)
    * originally written in Assembler for PDP-11
    * kernel was rewritten in C fairly early on
    * AT&T was monopoly so could not sell UNIX so instead gave to Universities
      for nominal fee.
* BSD (Berkeley Software Distribution)
    * BSD, 2BSD started as a set of tools to augment UNIX
    * 3BSD turned into a full fledged UNIX
    * 4.2BSD contained a full TCP/IP stack
        * became the basis for SunOS
        * Many Berkeley based unixes flourished in academia
* System III and System V
    * AT&T was broken up in '83 and started selling unix under System III
    * System V release 4 was licenced to many vendors and became basis of
        * HP UX
        * HP Tru64 UX
        * Digital Ultrix
        * SunOS and Solaris
        * IBM AIX
        * A/UX for Apple mac
        * SCO XENIX

So UNIX was portable across architectures and had less vendor lock-in so was
prefered.

Family trees:

* Gnu/Linux
    * Gnu project had almost a full free implementation of UNIZ except for the
      kernel
    * Gnu software was already running on existing unixen
    * Linus provided a kernel
    * linux was originally an attempt to make minix take advantage of 386
    * Since 2.6 they don't do the odd-numbered minor numbers are unstable thing
      anymore
    * See kernel version: `uname -a`
* Free BSDs
    * Eventually BSDs came around that had all the proprietary code re-written
    * This BSD fractured into multiple flavours of BSDs
        * FreeBSD
        * NetBSD
        * OpenBSD
* C language
    * C89 is still refered to as "ANSI C" even though technially ANSI C is the
      latest ratified version of the language.
    * C99
    * C2011
* POSIX
    * designed to provide cross-unix compatibility at the _source code level_ -
      POSIX does not specify binary compatibility
    * POSIX.1 - 1988
        * documents an API that should be made available by a "conforming" OS
          to a program
        * based on
            * unix system call API
            * C function API
    * many extensions to the standard have been defined e.g.
        * realtime
        * threads
        * shell utilities
    * Windows implemented POSIX.1 pre WinXP to fit US government regulations
        * They still have a half-hearted support for it which is not officially dead
    * POSIX.2
    * POSIX-1003.1-2001 == SUSv3 (Single unix specification)
        * Replaces a bunch of the older POSIX standards
    * POSIX.1-2008 == SUSv4 - 2008
        * iterates on SUSv3
        * currently this is most recent SUS standard

## Chapter 2

The typical name for the kernel on disk is `/boot/vmlinuz`

* In olden unix the kernel was `/boot/unix` so linux became `/boot/linux`
* With the advent of virtual memory it became `/boot/vmlinux`
* Then replaced the `x` with `z` to indicate that it is a compressed executable

* Linux does _preemptive multitasking_
    * processes get put on the CPU for a timeslice then taken off according to
      the kernel's schedule
* Modern CPUs allow operation in multipel modes
    * they have special asm instructions for switching between modes
    * common modes
        1. user mode
            * the **CPU** prevents access to kernel mode memory
            * the **CPU** prevents access to certain asm instructions e.g. halting system
        2. supervisor (or kernel) mode


A process

* may not necessarily be fully in memory at any one time
* a process is isolated
    * from its POV, all RAM is available to it
    * it has to ask the Kernel to do any interaction with the outside world
* a process cannot end itself (it has to ask the kernel to do it)
* the system calls API is how the process and kernel talk
* can ask the kernel
    * to end itself `_exit()`
    * to send receive/bytes from the filesystem
    * to send receive/bytes from the network
    * to duplicate itself `fork()`
* is the result of the kernel taking the bytes of a program from disk, putting
  them in memory and giving over some resources
* has a `cwd` pointer which holds the path to the "current
    working directory"
    * the login process sets its `cwd` from whatever `/etc/passwd` has as the
        home directory for that user
    * other processes inherits their cwd from their parent process but can
        change it
    * the shell is unusual in that it lets you (the user) explicitly change the
        cwd of the process via the `cd` builtin.

* CPU modes
    * CPU architectures have the idea of "modes" that the CPU can operate in.
    * CPU changes modes based on particular hardware instructions
    * Two important modes
        1. kernel/supervisor mode
            * all virtual memory available = kernel space
            * can initiate IO, access memory
        2. user mode
            * a subset of virtual memory is available (protected by the hardware) =
            user space
            * many operations are blocked e.g.
                * cannot halt the machine
                * cannot initiate IO
    * Each mode can have areas of virtual memory marked as being available from that
    mode
* virtual memory
    * virtual memory that can be seen from user mode = user space
    * virtual memory that can be seen from kernel mode = kernel space

* Exit code versus termination code:
    * if you exit explicitly via `exit()` then techically the integer returned is an exit code
    * if the process ends due to a signal or via any means other than `exit()` it is a termination code

Attempt to access virtual memory outside the allocated space raise a "hardware
exception". The typical configuration is

* kernel mode can see _all_ virtual memory
* user mode can see a restricted subset of virtual memory

Consider how the world looks from a process POV and from the kernel POV

* Process POV
    * a lot of asynchronous things happen to a process
        * does not know when it will be put on the CPU
        * does not know when its timeslice will elapse and it will be taken off CPU
        * does not know when signals will appear
        * does not know when IPC events will occur
    * does not know whether it lives in RAM or in swap (or both)
    * does not know where in RAM it really lives
    * does not know how to talk to other processes - it can only talk to the kernel who will do that communication on its behalf
* Kernel POV
    * knows which process will be on CPU next
    * knows how long that process will get the CPU for
    * does bookkeeping on processes as it makes changes to their life cycle
    * has the low-level data structures that map the VM seen by the process to
      real RAM and swap
    * Performs all IO through its device drivers e.g. disk driver, network driver
    * has the low-level data structures to present a disk of bytes as a file system

Shell

* also called command interpreter
* takes commands from user and executes programs
* takes output from programs and shows user
* is a user process
* can run interactive commands and scripts
* examples
    * sh (bourne shell, the original)
    * csh (came from Berkeley, C-alike syntax, superset of sh)
    * ksh (korn shell, superset of sh)
    * bash (gnu reimplementation and superset of sh)
    * dash (Debian Almquist Shell, mostly subset of bash => faster startup)
        * posix only, no XSI extensions
        * not intended for interactive use
        * intended for fast boottime startup scripts
* `sh` is provided by `dash` on Ubuntu (`bash` on other distros)

Login shell

> the process that is created to _run a shell_ when the user first logs in

??? is it not a shell itself ???

Users and groups

* Each user has a unique ID
* Each user has one primary group and 0-many secondary groups
    * primary group is mentioned in `/etc/passwd`
    * probably comes from how in early unix a user could only be one group
    * other groups are defined in `/etc/group`
        * `/etc/group` is designed to be combined with the group info in
          `/etc/passwd` to give a full picture of users and their groups

Filesystem

* On unix is a _single heirarchy_ (unlike windows where there is one heirarchy
  per disk)
* Filetypes
    1. plain files
    2. directories
    3. sockets
    4. pipes
    5. devices
    6. symbolic links
* Directories are files
    * can visualise as a simple spreadsheet with the following cols
        * filename
        * pointer to its inode (hard link)
        * filetype
        * ??? others? TODO: dig into the contents of a dir structure in a few filesystems
    * the same file can appear in multiple directory "tables" so can have multiple links
* when I `cd` in a shell I am changing which directory file inode the `cwd` for
  the shell process points to
* `..` in `/` points back to `/`
    * this means that you can "overuse" `../` to get up out of the current dir
      to the root dir i.e. you don't have to use the exact right number of
      `../`
* The 65 "safe" characters for filenames are `\_a-zA-Z0-9.` aka the "portable character set"
* how symbolic links are handled
    * normally the kernel automatically dereferences them, replacing each symlink in a path with the real filename to which it points
    * if a symlink points to a file which does not exist it is said to be "dangling"

QUESTION: how do I recognise when file has multiple hardlinks to it?

Recognising files which have multiple hardlinks

```
# I have three hardlinks to the same file in the current dir
# notice that the number of hardlinks is listed between the access bits and the
# owner name
$ ls -l *.jpg
-rw-r--r-- 3 eoinkelly staff 180331 May 25 10:53 cows.jpg
-rw-r--r-- 3 eoinkelly staff 180331 May 25 10:53 other_cows.jpg
-rw-r--r-- 3 eoinkelly staff 180331 May 25 10:53 yet_more_cows.jpg
# regular files have one hardlink by default

# gnu find can show you all the hardlinks to a particular file
$ find . -samefile /path/to/file

# if you rm a file with multiple hardlinks then the file is only actually
# deleted when you remove the last hardlink
$ rm other_cows.jpg
$ ls -l *.jpg
-rw-r--r-- 3 eoinkelly staff 180331 May 25 10:53 cows.jpg
-rw-r--r-- 3 eoinkelly staff 180331 May 25 10:53 yet_more_cows.jpg

# QUESTION: directories have more than one hardlinks by default - why ???
    also how to predict how many hardlinks a dir will have ???
```

Environment

* maintained in user-space memory
* when a new process is created its environment is initialized to be the same
  as its parent at the time of creation.
    * The child gets a _copy_ of the parent environment not a reference to it!
    * The `exec\*` calls (which replace a processes executable code with new
      code) can choose whether to keep the parent environment or make changes
* available to C programs using the "external" variable `(char **environ)`
    * see `environment.c` for some playing around with this

Resource limits

* a process can set upper limits on its own resource usage - it is "self regulated"
* implemented using the `setrlimit()` system call
* each resource limit has a
    * soft limit
        * the process can change its limit for a particular resource from
          0->hard-limit
        * this is the limit the kernel will enforce
    * hard limit
        * the ceiling that the soft-limit may be raised
        * can be irreversibly lowered by an unprivileged process
        * priveleged processes can change hard limit however they want
    * limits can be set to an "infinity" value
* when a process copies itself with `fork()` the new "child" copy inherits these limits
    * QUESTION: I assume this means a parent can set hard-limit and then
        spawn workers who must obey it?
* resources which can be limited
    * max size of process virtual memory in bytes
    * max core size
    * max CPU time in seconds
    * max size of data segment (initialized data, uninitialized data, heap)
    * max no. of locks the process can take out
    * max size of files the proces can create
    * max no. of bytes which may be locked in memory
    * max size of stack in bytes
    * other stuff ... `man setrlimit` for the full story

* you can change the limits of your shell process using `ulimit`
    * this will change the limits of the processes that the shell creates

```
# linux
$ ulimit -a
core file size          (blocks, -c) 0
data seg size           (kbytes, -d) unlimited
scheduling priority             (-e) 0
file size               (blocks, -f) unlimited
pending signals                 (-i) 1913
max locked memory       (kbytes, -l) 64
max memory size         (kbytes, -m) unlimited
open files                      (-n) 1024
pipe size            (512 bytes, -p) 8
POSIX message queues     (bytes, -q) 819200
real-time priority              (-r) 0
stack size              (kbytes, -s) 8192
cpu time               (seconds, -t) unlimited
max user processes              (-u) 1913
virtual memory          (kbytes, -v) unlimited
file locks                      (-x) unlimited

# mac
$ ulimit -a
-t: cpu time (seconds)              unlimited
-f: file size (blocks)              unlimited
-d: data seg size (kbytes)          unlimited
-s: stack size (kbytes)             8192
-c: core file size (blocks)         0
-v: address space (kbytes)          unlimited
-l: locked-in-memory size (kbytes)  unlimited
-u: processes                       709
-n: file descriptors                2560
```

mmap

* `mmap()` system call creates a new memory mapping

* file mapping
    * maps _a region_ of a file into the calling process's virtual memory
    * the VM system takes care of loading the right pages of the mapping into
      RAM when you need them
* anonymous mapping
    * not backed by any file
    * pages in the mapping are initialized to 0

When a mapping is created it can specify what access permissions should be
enforced on the pages of the mapping:

    * none
    * read
    * write
    * execute

Mappings have options - examples of which are:

* MAP_PRIVATE
    * modifications to the mapping of pages are "private" i.e. copy on
        write
        * i.e. processes can share pages until they modify them - at that
            point the system will make a new copy of the page for the process
    * other processes cannot see the modifications
    * modifications are not carried through to the file
* MAP_SHARED
    * modifications to the mapping are visible to other processes who also
        map this file
    * other processes can see the modifications
    * modifications are carried through to the file
* MAP_NOCACHE
    * pages in this mapping are not kept in kernel memory cache

Things memory mapping lets us do

1. memory mapping is how the code from the ELF binary is loaded into the
   process's TEXT segment
1. allocate new 0 filled memory
1. file I/O
1. Interprocess communication (via shared mapping)

An "object library" is a file containing the compiled object code for a set of functions

There are two types of _object library_

1. static library
    * also called "archive"
    * a bundle of compiled object modules (.o files)
    * when a `.a` file is linked with a program
    1. the linker resolves all the symbols that the library provides (function names, variable names etc.) that the program needs
    2. it then _copies_ those object modules into the program executable - this is _static linking_
    * static linking = copy and paste at compile time
2. shared library
    * linker writes a record into the executable to indicate which object libraries the program needs at runtime
    * when the program is run, a separate program called the "dynamic linker" makes sure the library required is loaded into memory and the symbols resolved

Linux has 7 Interprocess communication methods

1. signals
2. pipes and FIFOs
3. sockets
4. file locking
    * locks can be obtained for part of a file (does not need to be whole file)
5. message queues
6. semaphores
7. shared memory

There are so many because of the messy history of UNIX e.g. domain sockets
(SYSV) and FIFOs (BSD) are pretty much the same.


Signals

* software interrupts
* have lots of uses
* each signal is identified by a different int and has a symbolic name SIGXXX
    * `kill -l` to see signal names
* signal sources:
    * the kernel
    * another process (with suitable permissions)
    * the process itself
* `kill` shell command sends signals to processes. `kill()` system call does
  the same
* many shells also have a builtin kill command
    * use `/bin/kill` explicitly to avoid the builtin if you need to.
        * QUESTION: any reason to do this ???

How a signal is delivered to a process:

1. signal source sends signal to the process via `kill()`
2. kernel starts delivery if sender has permissions to send that signal
3. signal is "pending" for recipient, waiting for recipient to get on CPU again
4. kernel checkes "signal mask" for the recipeint - if this signal is blocked
   then the signal remains pending
5. signal is delivered when it is not blocked by mask anymore and the recipient
   is on the CPU

When a process receives a signal it can

1. run a custom "signal handler" function it registered earlier in its life
1. ignore the signal
1. take the default action for that signal e.g. kill or suspend itself

UP TO 2.12 THREADS

## Chapter 3

Always the return status of a system call!

* `man syscalls`
* system calls are usually not invoked directly but rather via wrapper
  functions in glibc
* the wrapper
    * often has same name as the call it wraps
    * is often thin
* the wrapper does
    1. puts its args in the right registers
    1. invoke the system call
    1. get the return value from the system call and then:
            * returns 0 if call was success
            * negates any error Integer it got from the syscall and puts it in `errno`
            * returns -1 to indicate that caller should look in `errno`


UP TO FIRST PAGE CHAP 3


# Chapter 6: Processes

A process is:

* An instance of an executing program

A "program" is a file containing the information that the kernel needs to create a process
* A program is a blueprint for a process

Program files containing

1. Binary format identification
1. Machine language instructions
1. Program entry-point address
    * The location of the first instruction that the process crreated from this program should execute
1. data
  * the values required to initialize variables and constants in the program
1. Symbol and relocation tables
    * These tables contain the locations of variables and functions used by the program
1. Shared library and dynamic linking information
    * fields listing the shared libraries the program needs at runtime
    * the pathname to the linker
1. Other info
    * misc other stuff

## Aside: size utility

Prints the sizes of the text, initialized data (data), uninitialized data (bss) sections in a binary

* The

```sh
# linux
vagrant@vagrant-ubuntu-vivid-64:~$ size  -t /bin/date
   text    data     bss     dec     hex filename
  58889    1236     448   60573    ec9d /bin/date

vagrant@vagrant-ubuntu-vivid-64:~$ size --format=SysV  /bin/date
/bin/date  :
section               size      addr
.interp                 28   4194872
.note.ABI-tag           32   4194900
.note.gnu.build-id      36   4194932
.gnu.hash               72   4194968
.dynsym               1944   4195040
.dynstr                849   4196984
.gnu.version           162   4197834
.gnu.version_r         112   4198000
.rela.dyn              192   4198112
.rela.plt             1704   4198304
.init                   26   4200008
.plt                  1152   4200048
.text                31786   4201200
.fini                    9   4232988
.rodata              15689   4233024
.eh_frame_hdr          764   4248716
.eh_frame             4332   4249480
.init_array              8   6352400
.fini_array              8   6352408
.jcr                     8   6352416
.dynamic               464   6352424
.got                     8   6352888
.got.plt               592   6352896
.data                  148   6353504
.bss                   448   6353664
.gnu_debuglink          12         0
Total                60585
```

```sh
# mac

➜  linux-api git:(master) size  /bin/date
__TEXT  __DATA  __OBJC  others  dec hex
12288   4096    0   4294975488  4294991872  100006000
➜  linux-api git:(master) size -m -l /bin/date
Segment __PAGEZERO: 4294967296 (vmaddr 0x0 fileoff 0)
Segment __TEXT: 12288 (vmaddr 0x100000000 fileoff 0)
    Section __text: 6305 (addr 0x100000e1c offset 3612)
    Section __stubs: 270 (addr 0x1000026be offset 9918)
    Section __stub_helper: 466 (addr 0x1000027cc offset 10188)
    Section __const: 344 (addr 0x1000029a0 offset 10656)
    Section __cstring: 1062 (addr 0x100002af8 offset 11000)
    Section __unwind_info: 140 (addr 0x100002f20 offset 12064)
    Section __eh_frame: 72 (addr 0x100002fb0 offset 12208)
    total 8659
Segment __DATA: 4096 (vmaddr 0x100003000 fileoff 12288)
    Section __got: 40 (addr 0x100003000 offset 12288)
    Section __nl_symbol_ptr: 16 (addr 0x100003028 offset 12328)
    Section __la_symbol_ptr: 360 (addr 0x100003038 offset 12344)
    Section __data: 552 (addr 0x1000031a0 offset 12704)
    Section __common: 8 (addr 0x1000033c8 offset 0)
    Section __bss: 8 (addr 0x1000033d0 offset 0)
    total 984
Segment __LINKEDIT: 8192 (vmaddr 0x100004000 fileoff 16384)
total 4294991872
➜  linux-api git:(master)
```
