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

```
root@ubuntu-bionic:/# cat /proc/interrupts
           CPU0       CPU1
  0:         38          0   IO-APIC   2-edge      timer
  1:          0          9   IO-APIC   1-edge      i8042
  4:          0       1734   IO-APIC   4-edge      ttyS0
  8:          0          0   IO-APIC   8-edge      rtc0
  9:          0          0   IO-APIC   9-fasteoi   acpi
 12:        156          0   IO-APIC  12-edge      i8042
 14:          0          0   IO-APIC  14-edge      ata_piix
 15:          0          0   IO-APIC  15-edge      ata_piix
 19:      10860       1089   IO-APIC  19-fasteoi   enp0s3
 20:       5198       2536   IO-APIC  20-fasteoi   ioc0, vboxguest
NMI:          0          0   Non-maskable interrupts
LOC:      20289      19450   Local timer interrupts
SPU:          0          0   Spurious interrupts
PMI:          0          0   Performance monitoring interrupts
IWI:          0          0   IRQ work interrupts
RTR:          0          0   APIC ICR read retries
RES:       6450       6149   Rescheduling interrupts
CAL:       2808       3319   Function call interrupts
TLB:         41         36   TLB shootdowns
TRM:          0          0   Thermal event interrupts
THR:          0          0   Threshold APIC interrupts
DFR:          0          0   Deferred Error APIC interrupts
MCE:          0          0   Machine check exceptions
MCP:          5          5   Machine check polls
HYP:          0          0   Hypervisor callback interrupts
ERR:          0
MIS:          0
PIN:          0          0   Posted-interrupt notification event
NPI:          0          0   Nested posted-interrupt event
PIW:          0          0   Posted-interrupt wakeup event
```


## Proc & sysfs

Proc filesystem

* `man proc`
* a process information pseudo-filesystem aka virtual filesystem
* it is a read/write window into the kernel
* userland tools think they are working with files so they issue normal "file" system calls. Under the hood the kernel knows what to do when it gets system calls to CRUD files under `/proc`
* all files have size 0 bytes except:
    * mtrr
    * kcore
    * self
* mostly it contains numbered subdirs which represent processes
    * each subdir contains a bunch of info about that process
    * `cwd` is a link to the cwd of the process
    * `exe` is a link to the executable that created the process
    * `environ` a file containing the env of the process
    * `cmdline` a file containing the command line used to run the process
* mounted under `/proc`
* Proc filesystem is common to many unixes
* Linux added other stuff but later kernels moved that stuff to sysfs
* contains runtime information about the kernel that can be queried by tools

```
$ lsmod
$ cat /proc/modules

$ uname -a
$ cat /proc/version


root@ubuntu-bionic:/# procinfo
Memory:        Total        Used        Free     Buffers
RAM:         1008940      521528      487412       16084
Swap:              0           0           0

Bootup: Thu Feb 28 17:54:35 2019   Load average: 0.12 0.03 0.01 1/123 2345

user  :   00:00:18.25   0.7%  page in :           182150
nice  :   00:00:02.80   0.1%  page out:           396448
system:   00:00:10.53   0.4%  page act:            85561
IOwait:   00:00:06.58   0.3%  page dea:                0
hw irq:   00:00:00.00   0.0%  page flt:           915981
sw irq:   00:00:01.83   0.1%  swap in :                0
idle  :   00:40:43.51  98.4%  swap out:                0
uptime:   00:20:46.53         context :           101698

irq   0:         38  2-edge timer        irq  12:        156  12-edge i8042
irq   1:          9  1-edge i8042        irq  14:          0  14-edge ata_piix
irq   4:       1734  4-edge ttyS0        irq  15:          0  15-edge ata_piix
irq   8:          0  8-edge rtc0         irq  19:      11458  19-fasteoi enp0s3
irq   9:          0  9-fasteoi acpi      irq  20:       7551  20-fasteoi ioc0,

sda             6893r            1846w   sdb              177r               0w

enp0s3      TX 541.79KiB     RX 29.37MiB      lo          TX 3.62KiB       RX 3.62KiB
```

Q: how do i see the environment that a process has witout using htop?
A: `cat /proc/PID/environ` or `ps e PID`

Sys filesystem

* a virtual filesystem
* mounted under `/sys`


