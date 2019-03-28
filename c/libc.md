# C Standard Library (libc)

## Documentation

* https://en.wikipedia.org/wiki/C_standard_library (good overview)
* https://www.gnu.org/software/libc/documentation.html
    * https://www.gnu.org/software/libc/manual/ (canonical glibc manual)
    * https://www.gnu.org/software/libc/manual/pdf/libc.pdf (manual in PDF)
    * https://www.gnu.org/software/libc/manual/html_mono/libc.html (this is the most useful version of the doc, easily searchable)
* Functions from libc are documented in section 3 of the man pages
    * these man pages are not canonically part of the glibc project (but glibc devs do work closely with the man pages projects)
	* Example: `man glibc` (very short overview, not super useful tbh)
    * there is no full overview of libc as a man page - you have to use the official manual

## Find your current glibc version

Find glibc version by running the libc so archive as a binary:

```
$ /lib/x86_64-linux-gnu/libc.so.6
GNU C Library (Ubuntu GLIBC 2.27-3ubuntu1) stable release version 2.27.
Copyright (C) 2018 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.
There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.
Compiled by GNU CC version 7.3.0.
libc ABIs: UNIQUE IFUNC
For bug reporting instructions, please see:
<https://bugs.launchpad.net/ubuntu/+source/glibc/+bugs>.
```

## Background

* The C _language_ doesn't provide much - no memory handling, string handling, I/O
* The language relies on libc being available at runtime
* C standard library provides
    1. macros
    1. type definitions
    1. functions
    1. variables
* You can skip including header files if you typed out the declarations
  yourself at the top of your file. This allows the compiler to complete but
  risks your declaration being out of date with what is available on the system.
* C11 has 29 header files. C99 has 23
    * See https://en.wikipedia.org/wiki/C_standard_library for table
* glibc includes functions which are part of ISO C but it also complys with other standards which extend that group:
    * ISO C
        * If you compile with `gcc -ansi` you get only ISO C features
    * POSIX C is a superset of ISO C
        * POSIX adds new features to existing ISO C functions
        * POSIX adds new functions not found in ISO C
        * POSIX adds serveral header files e.g.
            * `signal.h`
            * `unistd.h`
    * SVID (System V Interface description)
        * is mostly overlap with POSIX & ISO C but glibc does comply with most of it
    * BSD unix
        * BSD defines a number of features which aren't part of ISO or POSIX C
        * examples
            * symbolic links
            * `select` function for IO
            * sockets
            * BSD signal functions
    * X/Open portability guide
        * is a superset standard than POSIX
        * contains stuff that is mostly from SVID and BSD
    * Gnu C library also contains headers unique to it (i.e. they are not in any) e.g.
        * `alloca.h`
* Unix systems consider libc to be part of the OS
    * The kernel does not use libc but it provides an interface for applications to talk to the kernel
* Windows does not consider its C library to be an interface to the OS
* Some functions in libc have macro versions too to allow them to be inlined
	* inlined functions can be optimized better by compiler because it can understand flow control better
	* these inlined functions can be confusing when you are debugging
	* you can individual `#undef` a particular marcro if you want to debug it as a real function
* The error handling of the functions in the C standard library is not consistent and sometimes confusing.
	* According to the Linux manual page math_error,
        > "The current (version 2.8) situation under glibc is messy. Most (but
        > not all) functions raise exceptions on errors. Some also set errno. A
        > few functions set errno, but don't raise an exception. A very few
        > functions do neither."
* Other languages can wrap libc
    * The C++ language, for example, includes the functionality of the C standard library in the `std` namespace
    * Rust has a crate called `libc` which allows several C functions, structs, and other type definitions to be used
* Interestingly go does not use libc
    * Go completely bypasses libc, the Go runtime and stdlib use syscalls directly
    * The Go runtime is analagous to libc except it is more central to the language
        * The Go runtime is not a VM. Go code is compiled to native code.

## libc doc safety annotations

The doc has a bunch of flags for each function which indicate whether it is safe to use in certian conditions

Defn: atomic ???
Q: what does "configure the floating point environment" mean?

1. MT-Safe (Multi-thread safe)
    * this function is safe to call in the presense of other threads
    * there are important caveats on this:
        * MT-Safe does not mean the function is atomic
        * MT-Safe does not mean that this function uses any of the memory synchronisation features that POSIX offers
        * MT-Safe does not mean that calling two MT-Safe functions in a row will give you an equivalent of atomic execution of both functions because concurrent calls in other threads can interfere
1. AS-Safe (Async Signal safe)
    * this function is safe to call from an asynchronous signal handler
1. AC-Safe (Async Cancel safe)
    * this function is safe to call when asynchronous cancellation is enabled

If the current implementation of a function is safe in some way but the standard which defines that function does not **require** that safety then this is documented as a _Preliminary safety property_. The doc says that you should not rely on that safety in future (I'm not sure how stable that safety is in reality)

TODO: more to do on safety annotations

* Reserved names
    * keywords like `exit`
    * all external identifiers (global functions and variables) that begin with an underscore (‘_’)
        * `extern _foo`, `extern _bar`
    * all identifiers (regardless of use) that begin with either two underscores or an underscore followed by a capital letter
        * `__Foo` or `__FOO`
    * E followed by a digit or uppercase
        * `E111`, `EFoo`, `EFOO11`
    * names which begin with `is` or `to` followed by a lowercase letter
        * `isfoo`, `tofoo`
    * names beginning with
        * `SIG_`
        * `SIG`
        * `LC_`
    * math function names suffixed by `f` or `l`
    * names ending iwth `_t` are reserved for additional type names
    * names beginning with `wcs`, `mem`, `str`



### Feature test macros

Glibc can be configured by feature test flags implemented as macros
* Macros can be defined in 2 ways
    1. at the top of your source code (before any `#include`)
    1. via the `-D` command line flag to gcc
* Many macros begin with an `_` presumably to make the command line version look good e.g. `-D_POSIX_SOURCE`



## NULL

* the _null pointer constant_ is guaranteed not to point to a real object
*
* `NULL` is the preferred way to write it. It has type `void *` so should only be assigned to **pointers**

```c
// all these are valid ways of writing the null pointer constant
NULL // preferred
(void *)0
0
```


