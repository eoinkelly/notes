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


## Processes

```
vagrant@ubuntu-bionic:~$ pmap -X 1457
1457:   -bash
         Address Perm   Offset Device Inode  Size  Rss  Pss Referenced Anonymous LazyFree ShmemPmdMapped Shared_Hugetlb Private_Hugetlb Swap SwapPss Locked Mapping
    563bf0da3000 r-xp 00000000  08:01    31  1040  944  944        944         0        0              0              0               0    0       0    944 bash
    563bf10a6000 r--p 00103000  08:01    31    16   16   16         16        16        0              0              0               0    0       0     16 bash
    563bf10aa000 rw-p 00107000  08:01    31    36   36   36         36        36        0              0              0               0    0       0     36 bash
    563bf10b3000 rw-p 00000000  00:00     0    40   28   28         28        28        0              0              0               0    0       0     28
    563bf1930000 rw-p 00000000  00:00     0  1520 1480 1480       1480      1480        0              0              0               0    0       0   1480 [heap]
    7f522b4ce000 r-xp 00000000  08:01  2081    44   44    2         44         0        0              0              0               0    0       0      2 libnss_files-2.27.so
    7f522b4d9000 ---p 0000b000  08:01  2081  2044    0    0          0         0        0              0              0               0    0       0      0 libnss_files-2.27.so
    7f522b6d8000 r--p 0000a000  08:01  2081     4    4    4          4         4        0              0              0               0    0       0      4 libnss_files-2.27.so
    7f522b6d9000 rw-p 0000b000  08:01  2081     4    4    4          4         4        0              0              0               0    0       0      4 libnss_files-2.27.so
    7f522b6da000 rw-p 00000000  00:00     0    24    0    0          0         0        0              0              0               0    0       0      0
    7f522b6e0000 r-xp 00000000  08:01  2078    92   64    4         64         0        0              0              0               0    0       0      4 libnsl-2.27.so
    7f522b6f7000 ---p 00017000  08:01  2078  2044    0    0          0         0        0              0              0               0    0       0      0 libnsl-2.27.so
    7f522b8f6000 r--p 00016000  08:01  2078     4    4    4          4         4        0              0              0               0    0       0      4 libnsl-2.27.so
    7f522b8f7000 rw-p 00017000  08:01  2078     4    4    4          4         4        0              0              0               0    0       0      4 libnsl-2.27.so
    7f522b8f8000 rw-p 00000000  00:00     0     8    0    0          0         0        0              0              0               0    0       0      0
    7f522b8fa000 r-xp 00000000  08:01  2083    44   44    2         44         0        0              0              0               0    0       0      2 libnss_nis-2.27.so
    7f522b905000 ---p 0000b000  08:01  2083  2044    0    0          0         0        0              0              0               0    0       0      0 libnss_nis-2.27.so
    7f522bb04000 r--p 0000a000  08:01  2083     4    4    4          4         4        0              0              0               0    0       0      4 libnss_nis-2.27.so
    7f522bb05000 rw-p 0000b000  08:01  2083     4    4    4          4         4        0              0              0               0    0       0      4 libnss_nis-2.27.so
    7f522bb06000 r-xp 00000000  08:01  2079    32   32    1         32         0        0              0              0               0    0       0      1 libnss_compat-2.27.so
    7f522bb0e000 ---p 00008000  08:01  2079  2048    0    0          0         0        0              0              0               0    0       0      0 libnss_compat-2.27.so
    7f522bd0e000 r--p 00008000  08:01  2079     4    4    4          4         4        0              0              0               0    0       0      4 libnss_compat-2.27.so
    7f522bd0f000 rw-p 00009000  08:01  2079     4    4    4          4         4        0              0              0               0    0       0      4 libnss_compat-2.27.so
    7f522bd10000 r--p 00000000  08:01  7782  1484  136   28        136         0        0              0              0               0    0       0     28 LC_COLLATE
    7f522be83000 r-xp 00000000  08:01  2071  1948 1588   66       1588         0        0              0              0               0    0       0     66 libc-2.27.so
    7f522c06a000 ---p 001e7000  08:01  2071  2048    0    0          0         0        0              0              0               0    0       0      0 libc-2.27.so
    7f522c26a000 r--p 001e7000  08:01  2071    16   16   16         16        16        0              0              0               0    0       0     16 libc-2.27.so
    7f522c26e000 rw-p 001eb000  08:01  2071     8    8    8          8         8        0              0              0               0    0       0      8 libc-2.27.so
    7f522c270000 rw-p 00000000  00:00     0    16   16   16         16        16        0              0              0               0    0       0     16
    7f522c274000 r-xp 00000000  08:01  2074    12   12    0         12         0        0              0              0               0    0       0      0 libdl-2.27.so
    7f522c277000 ---p 00003000  08:01  2074  2044    0    0          0         0        0              0              0               0    0       0      0 libdl-2.27.so
    7f522c476000 r--p 00002000  08:01  2074     4    4    4          4         4        0              0              0               0    0       0      4 libdl-2.27.so
    7f522c477000 rw-p 00003000  08:01  2074     4    4    4          4         4        0              0              0               0    0       0      4 libdl-2.27.so
    7f522c478000 r-xp 00000000  08:01  2187   148  148  148        148         0        0              0              0               0    0       0    148 libtinfo.so.5.9
    7f522c49d000 ---p 00025000  08:01  2187  2048    0    0          0         0        0              0              0               0    0       0      0 libtinfo.so.5.9
    7f522c69d000 r--p 00025000  08:01  2187    16   16   16         16        16        0              0              0               0    0       0     16 libtinfo.so.5.9
    7f522c6a1000 rw-p 00029000  08:01  2187     4    4    4          4         4        0              0              0               0    0       0      4 libtinfo.so.5.9
    7f522c6a2000 r-xp 00000000  08:01  2067   156  156    5        156         0        0              0              0               0    0       0      5 ld-2.27.so
    7f522c6e7000 r--p 00000000  08:01  7783   196   96   18         96         0        0              0              0               0    0       0     18 LC_CTYPE
    7f522c718000 r--p 00000000  08:01  7788     4    4    0          4         0        0              0              0               0    0       0      0 LC_NUMERIC
    7f522c719000 r--p 00000000  08:01  7791     4    4    0          4         0        0              0              0               0    0       0      0 LC_TIME
    7f522c71a000 r--p 00000000  08:01  7786     4    4    0          4         0        0              0              0               0    0       0      0 LC_MONETARY
    7f522c71b000 r--s 00000000  08:01  4998    28   28    3         28         0        0              0              0               0    0       0      3 gconv-modules.cache
    7f522c722000 r--p 00000000  08:01  7777  1644   64    9         64         0        0              0              0               0    0       0      9 locale-archive
    7f522c8bd000 rw-p 00000000  00:00     0    20   16   16         16        16        0              0              0               0    0       0     16
    7f522c8c2000 r--p 00000000  08:01  7780     4    4    0          4         0        0              0              0               0    0       0      0 SYS_LC_MESSAGES
    7f522c8c3000 r--p 00000000  08:01  7789     4    4    0          4         0        0              0              0               0    0       0      0 LC_PAPER
    7f522c8c4000 r--p 00000000  08:01  7787     4    4    0          4         0        0              0              0               0    0       0      0 LC_NAME
    7f522c8c5000 r--p 00000000  08:01  7781     4    4    0          4         0        0              0              0               0    0       0      0 LC_ADDRESS
    7f522c8c6000 r--p 00000000  08:01  7790     4    4    0          4         0        0              0              0               0    0       0      0 LC_TELEPHONE
    7f522c8c7000 r--p 00000000  08:01  7785     4    4    0          4         0        0              0              0               0    0       0      0 LC_MEASUREMENT
    7f522c8c8000 r--p 00000000  08:01  7784     4    4    0          4         0        0              0              0               0    0       0      0 LC_IDENTIFICATION
    7f522c8c9000 r--p 00027000  08:01  2067     4    4    4          4         4        0              0              0               0    0       0      4 ld-2.27.so
    7f522c8ca000 rw-p 00028000  08:01  2067     4    4    4          4         4        0              0              0               0    0       0      4 ld-2.27.so
    7f522c8cb000 rw-p 00000000  00:00     0     4    4    4          4         4        0              0              0               0    0       0      4
    7ffe465c5000 rw-p 00000000  00:00     0   132   28   28         28        28        0              0              0               0    0       0     28 [stack]
    7ffe465f5000 r--p 00000000  00:00     0    12    0    0          0         0        0              0              0               0    0       0      0 [vvar]
    7ffe465f8000 r-xp 00000000  00:00     0     8    4    0          4         0        0              0              0               0    0       0      0 [vdso]
ffffffffff600000 r-xp 00000000  00:00     0     4    0    0          0         0        0              0              0               0    0       0      0 [vsyscall]
                                            ===== ==== ==== ========== ========= ======== ============== ============== =============== ==== ======= ======
                                            23160 5116 2946       5116      1716        0              0              0               0    0       0   2946 KB
vagrant@ubuntu-bionic:~$ ls -l /bin/bash
```

htop says for the same bash process:

VIRT 23160 (matches sum of 'Size' column in pmap output
RES 5116 (matches sum of 'Rss' column in pmap output
SHR 3400 how is this calcluated ???

