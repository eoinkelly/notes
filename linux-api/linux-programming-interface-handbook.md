# Linux Programming Interface Book

## Aside: Building C files


```sh
# Works on Mac and Linux

vim foo.c

gcc foo.c -o exe_name
./exe_name

# make can build a single file without a Makefile
make foo
./foo
```

## Aside: Tracing system calls

strace = dtruss

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
* `otool -L` is equivalent on mac

```sh
ldd /bin/date # linux
otool -L /bin/date # mac
```

## Aside: LibC on Ubuntu Vivid

```sh
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
<https://bugs.launchpad.net/ubuntu/+source/glibc/+bugs>.
```

## Preface

Standards

* POSIX.1-2001 (Portable Operating System Interface)
* SUSv3 (Single UNIX Specification version 3)

* SUSv4 (Single UNIX Specification version 3)
* POSIX.1-2008

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
3
4. 32
5. 221
6. 1
7. 244
8. 406
9. 5

## Chapter 1

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
        * TODO: find out which version of C is most widely used
    * C2011
* POSIX
    * designed to provide cross-unix compatibility at the source code level
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

* In olden unix the kernel was `/boot/unix`
* With the advent of VM it became `/boot/vmunix`
* Replace `x` with `z` to indicate that it is compressed

* Linux does _preemptive multitasking_
    * processes get put on the CPU for a timeslice then taken off according to
      the kernel's schedule

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
* is the result of the kernel taking the bytes of a program from disk, putting them in memory and giving over some resources
* cwd
    * has a `cwd` pointer which holds the path to the processes "current
      working directory"
    * the login process sets its `cwd` from whatever `/etc/passwd` has as the
      home directory for that user
    * other processes inherits its cwd from its parent process

CPU architectures have the idea of "modes" that the CPU can operate in.
CPU changes modes based on particular hardware instructions
lTwo important modes

1. kernel/supervisor mode
    * all virtual memory available = kernel space
    * can initiate IO, access memory
2. user mode
    * a subset of virtual memory is available (protected by the hardware) =
      user space
    * many operations are blocked e.g.
        * cannot halt the machine
        * cannot initiate IO

Each mode can have areas of virtual memory marked as being available from that
mode

* virtual memory that can be seen from user mode = user space
* virtual memory that can be seen from kernel mode = kernel space

Attempt to access virtual memory outside the allocated space raise a "hardware
exception". The typical configuration is

* kernel mode can see _all_ virtual memory
* user mode can see a restricted subset of virtual memory

* Process POV
    * a lot of asynchronous things happen to a process
        * does not know when it will be put on the CPU
        * does not know when its timeslice will elapse and it will be taken off CPU
        * does not know when signals will appear
        * does not know when IPC events will occur
    * does not know whether it lives in RAM or in swap (or both)
    * does not know where in RAM it lives
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
    * the process that is created to _run a shell_
    * ??? is it not a shell itself?

Users and groups

* Each user has a unique ID
* Each user has one primary group and 0-many secondary groups
    * primary group is mentioned in `/etc/passwd
    * probably comes from how in early unix a user could only be one group
    * other groups are defined in `/etc/group`
        * `/etc/group` is designed to be combined with the group info in
          `/etc/passwd` to give a full picture of users and their groups

Filesystem

* On unix is a _single heirarchy_ (unlike windows where there is one heirarchy per disk)
* Filetypes
    * plain files
    * directories
    * sockets
    * pipes
    * devices
    * symbolic links

* Directories are files
    * can visualise as a simple spreadsheet with the following cols
        * filename
        * pointer to its inode
        * filetype
        * ??? others
    * each row in the spreadsheet is a hard link to a file
    * the same file can appear in multiple directory "tables" so can have
      multiple links
* when I `cd` in a shell I am changing which directory file inode the `cwd` for
  the shell process points to
* `..` in `/` points back to `/`
* The 65 "safe" characters are `\_a-zA-Z0-9.`


UP TO
FILE OWNERSHIP AND PERMISSIONS HEADING

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
