# Building kernels in vagrant

Using ubuntu bionic

# Linux Kernel Development

## What makes a unix unix

1. _Everything is a file_
    * Greatly simplifies IO - you can use the same few system calls to do IO with all kinds of devices
    * In Linux not _everything_ is a file but most things are (Aside: Plan9 really does have everything as a file)
2. Written in C
3. Implements fast process creation with `fork()`

## Anatomy

* Interrupt handlers
* scheduler
    * shares CPU time between multiple processes
* Memory management system
    * manages address space for processes
* System services
    * Networking
    * IPC

## Spaces

1. Kernel space
    * code has full access to hardware
    * access to a protected memory space
2. User space
    * no access to hardware (I think!)
    * no access to protected memory area

## Contexts

The kernel has 3 contexts it can be in at any one time

1. executing code in a process (user-space)
2. In _process context_, executing code **on behalf of** a specific process (kernel-space)
3. in _interrupt context_, executing code to handle a hardware interrupt (not associated with any process)

This list covers all corner cases e.g. when kernel is idle it is is actually in _process context_ executing code for an "idle process"

## System calls

* Linux has 326 system calls (as of kernel 3.0)
    * This low number is one of the reasons people describe unix as "simple"
    * See http://man7.org/linux/man-pages/man2/syscalls.2.html
* Applications rarely call system calls directly - the almost always use C library function wrappers
    * some wrappers are thin and basically just invoke the system call e.g. `open()`
    * some wrappers do a lot more e.g. `printf()` does buffering and formatting before eventually calling `write()`
* Obviously not all functions in the C library are wrappers around system calls
* When the kernel is executing a system call at an applications request it is said to be running in _process context_

```
# Most commonly used system calls

## System calls for IO

open()
read()
write()
lseek()
close()


fork()
```

## Interrupts

* When hardware wants to communicate with the CPU it issues an interrupt with interrupts the CPU and the CPU then interrupts the Kernel
* Each interrupt has a number and the kernel uses this to find the correct handler code to run in response
    * Example: typing on keyboard issues interrupts
* The kernel can disable all interrupts or just one specific interrupt number
    * It does this to "provide synchronisation"

