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

FINISHED UP TO END CHAP 1
TODO

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
