# Linux disk IO tools

## Terminology

### IO scheduling class and priority

Each process has:

1. An I/O scheduling _class_. One of:
    1. None (0)
        * processes are in this class if they are not specifically put in a different class
        * the kernel scheduler treats this class as if it were the same as `Best-Effort` i.e. if `None` is the reported class then `Best-Effort` is the effective class
    1. Realtime (1)
        * given first access to the disk regardless of what else is happening on the system
        * can starve other proesses
        * valid priority levels are 0-7 (0 = highest, 7 = lowest) within this class
        * only root can set this
    1. Best-effort (2)
        * valid priority levels are 0-7 (0 = highest, 7 = lowest) within this class
        * Priority defaults to being calculated by formula `io_priority = (cpu_nice + 20) / 5`
            * By default `cpu_nice = 0` for a process so by default `io_priority = (0 + 20) / 5 = 4`
        * In modern kernels processes inherit their IO scheduling **class** from their parent but their priority is calculated based on CPU niceness
    1. Idle (3)
        * this class is the lowest - processes in this class will only get disk time when **no other program** has asked for any within a defined grace period.
        * has no priority levels within it
1. An I/O scheduling _priority_
    * a number which defines how big a time slice a given process will receive on each scheduling window
    * Most (but not all) scheduling classes have priorities within them from 0 (highest) to 7 (lowest)

Examples

```
# using ionice to query I/O scheduling class and priority within that class

$ ionice -p 10327
none: prio 4

$ ionice -p 3212
none: prio 0
```

> Even with the lowest priority, a disk-intensive process tends to slow the system down, if nothing else because it pollutes the cache.

QUESTION: which cache?

## Tools

Common tools

1. top
2. perf
3. atop
4. iotop
5. iostat

Custom tools

* showboost
* pmcarch


### top

I/O wait is the percentage of time the CPU has to wait for the disk

Highi IO presents as

* load averages are high but CPUs don't report as busy doing either user or system work
* `top` by default reports `wa` on the `%Cpu(s)` line
    * `wa` is the average across all cores of the percent of time that core was waiting for IO when sampled
        * NB: it is an **average** so hides detail
    * Hit `1` in top to show stats for each CPU core - this is a more useful view
    * A `wa` value of `0.0` is expected, anything above `1.0` indicates IO bottleneck

### perf

* part of linux kernel
* can be used to do sampling of a specific running process
    * http://www.brendangregg.com/blog/2014-06-22/perf-cpu-sample.html

### atop

* runs as a background process - I'm not sure I want this on servers by default

```
Figures shown for active processes:
        'g' - generic info (default)
        'm' - memory details
        'd' - disk details
        'n' - network details
        's' - scheduling and thread-group info
        'v' - various info (ppid, user/group, date/time, status, exitcode)
        'c' - full command-line per process
        'o' - use own output line definition

Sort list of processes in order of:
        'C' - cpu activity
        'M' - memory consumption
        'D' - disk activity
        'N' - network activity
        'A' - most active system resource (auto mode)

Accumulated figures:
        'u' - total resource consumption per user
        'p' - total resource consumption per program (i.e. same process name)

Selections:
        'U' - focus on specific user name    (regular expression)
        'P' - focus on specific process name (regular expression)

Screen-handling:
        ^L  - redraw the screen
        ^F  - show next     page in the process-list (forward)
        ^B  - show previous page in the process-list (backward)

Presentation (these keys are shown in the header line):
        'a' - show all processes (default: active processes)   (toggle)
        'f' - fixate on static range of header-lines           (toggle)
        'x' - no colors to indicate high occupation            (toggle)
        '1' - show average-per-second i.s.o. total values      (toggle)

Raw file viewing:
        't' - show next     sample in raw file
        'T' - show previous sample in raw file
        'b' - branch to certain time in raw file)
        'r' - rewind to begin of raw file)

Miscellaneous commands:
        'i' - change interval-timer (0 = only manual trigger)
        't' - manual trigger to force next sample
        'r' - reset counters to boot time values
        'z' - pause-button to freeze current sample (toggle)

        'l' - limited lines for per-cpu, disk and interface resources
        'k' - kill a process (i.e. send a signal)

        'V' - version-information
        '?' - help-information
        'h' - help-information
        'q' - quit this program
```

### iotop

* shows percentage of sampling period where the thread/process was
    * swapping data into memory
    * waiting on I/O
* shows I/O priority (class/level)


```
sudo iotop -Po
```

```
Use the left and right arrows to change the sorting, r to reverse the  sorting
order, o to toggle the --only option, p to toggle the --processes option, a to
toggle the --accumulated option, q to quit or i to change the  priority  of  a
thread or a process' thread(s). Any other key will force a refresh.
```
