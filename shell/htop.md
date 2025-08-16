# htop

## Stats

Interpreting load averages

- Tasks = processes

## Usage graphs

1. CPU usage
    - purple: low-priority
    - green: normal
    - red: kernel
    - blue: virtualiz
2. Memory usage
    - green: used
    - purple: buffers
    - yellow: disk cache
3. Swap usage
    - red bars indicated swap (file/partition) used

## Columns

- PID
    - Process ID
- USER = User
    - username who owns the process
- PRI = Priority
    - ??? positive integer
    - on my mac, default value seems to be 17
    - on ubuntu xenial, default value seems to be 20
- NI = Nice
    - nice value of the process
    - defaults to 0 and will usuall be 0
- VIRT = Memory size
- RES = Memory resident
- SHR = Memory share
    - not visible on macOs
- S = State
    - R running
    - S sleeping
    - T traced/stopped
    - Z zombie
    - D disk sleep
        - QUESTION: what does this mean?
- percent CPU
- percent Mem
- Time
- Command
    - the name of the command
    - `p` to toggle full path
    - can highlight basename of command in setup screen

## Setup options

- "show custom thread names" ???

## Questions

    H    Hide user threads: on systems that represent them differently  than ordinary  processes  (such  as recent NPTL-based systems)
    QUESTION: which systems do that?

- Not all systems represent threads differently to processes.
    - linux seems to
    - macOs does not seem to

### Aside: macOs swap

- mac uses swap files not partition
- swap files and sleepimage lives in `/private/var/vm`
- swap space is usually split across multiple files e.g. my laptop has 2 x 1GB
  swap files

```
/private/var/vm $ ls -lh
 total 3.0G
 -rw------T 1 root wheel 1.0G May 20 07:22 sleepimage
 -rw------- 1 root wheel 1.0G May 19 09:28 swapfile0
 -rw------- 1 root wheel 1.0G May  5 11:43 swapfile1
```

## Keyboard shortcuts

```
Up, Alt-k
    Select (hightlight) the  previous  process  in  the  process  list.
    Scroll the list if necessary.

Down, Alt-j
    Select  (hightlight)  the  next process in the process list. Scroll
    the list if necessary.

Left, Alt-h
    Scroll the process list left.

Right, Alt-l
    Scroll the process list right.

PgUp, PgDn
    Scroll the process list up or down one window.

Home Scroll to the top of the process list and select the first process.

End  Scroll  to  the  bottom  of  the  process  list and select the last
    process.

Ctrl-A, ^
    Scroll left to the beginning of the process entry  (i.e.  beginning
    of line).

Ctrl-E, $
    Scroll right to the end of the process entry (i.e. end of line).

Space
    Tag  or untag a process. Commands that can operate on multiple pro-
    cesses, like "kill", will then apply over the list of  tagged  pro-
    cesses, instead of the currently highlighted one.

U    Untag all processes (remove all tags added with the Space key).

s    Trace  process  system  calls:  if strace(1) is installed, pressing
    this key will attach it to the currently selected process, present-
    ing a live update of system calls issued by the process.

l    Display open files for a process: if lsof(1) is installed, pressing
    this key will display the list of file descriptors  opened  by  the
    process.

F1, h, ?
    Go to the help screen

F2, S
    Go  to  the  setup  screen, where you can configure the meters dis-
    played at the top of  the  screen,  set  various  display  options,
    choose among color schemes, and select which columns are displayed,
    in which order.

F3, /
    Incrementally search the command lines of all  the  displayed  pro-
    cesses. The currently selected (highlighted) command will update as
    you type. While in search mode,  pressing  F3  will  cycle  through
    matching occurrences.

F4, \
    Incremental  process  filtering:  type in part of a process command
    line and only processes whose names match will be shown. To  cancel
    filtering, enter the Filter option again and press Esc.

F5, t
    Tree  view:  organize processes by parenthood, and layout the rela-
    tions between them as a tree. Toggling the key will switch  between
    tree  and your previously selected sort view. Selecting a sort view
    will exit tree view.

F6   On sorted view, select a field for sorting, also accessible through
    < and >.  The current sort field is indicated by a highlight in the
    header.  On tree view, expand or collapse the  current  subtree.  A
    "+" indicator in the tree node indicates that it is collapsed.

F7, ]
    Increase  the  selected  process's  priority  (subtract from 'nice'
    value).  This can only be done by the superuser.

F8, [
    Decrease the selected process's priority (add to 'nice' value)

F9, k
    "Kill" process: sends a signal which is selected in a menu, to  one
    or a group of processes. If processes were tagged, sends the signal
    to all tagged processes.  If none is tagged, sends to the currently
    selected process.

F10, q
    Quit

I    Invert  the  sort  order:  if  sort  order is increasing, switch to
    decreasing, and vice-versa.

+, - When in tree view mode, expand or collapse subtree. When a  subtree
    is collapsed a "+" sign shows to the left of the process name.

a (on multiprocessor machines)
    Set CPU affinity: mark which CPUs a process is allowed to use.

u    Show only processes owned by a specified user.

M    Sort by memory usage (top compatibility key).

P    Sort by processor usage (top compatibility key).

T    Sort by time (top compatibility key).

F    "Follow"  process:  if the sort order causes the currently selected
    process to move in the list, make the selection bar follow it. This
    is  useful  for  monitoring  a  process:  this  way, you can keep a
    process always visible on screen. When  a  movement  key  is  used,
    "follow" loses effect.

K    Hide kernel threads: prevent the threads belonging the kernel to be
    displayed in the process list. (This is a toggle key.)

H    Hide user threads: on systems that represent them differently  than
    ordinary  processes  (such  as recent NPTL-based systems), this can
    hide threads from userspace processes in the process list. (This is
    a toggle key.)
p    Show  full  paths to running programs, where applicable. (This is a
    toggle key.)

Ctrl-L
    Refresh: redraw screen and recalculate values.

Numbers
    PID search: type in process ID and the selection highlight will  be
    moved to it.

```

## Interesting bits from the FAQ

> The memory meter in htop says a low number, such as 9%, when top shows
> something like 90%! (Or: the MEM% number is low, but the bar looks almost
> full. What's going on?)
>
> The number showed by the memory meter is the total memory used by processes.
> The additional available memory is used by the Linux kernel for buffering and
> disk cache, so in total almost the entire memory is in use by the kernel. I
> believe the number displayed by htop is a more meaningful metric of resources
> used: the number corresponds to the green bars; the blue and brown bars
> correspond to buffers and cache, respectively (as explained in the Help screen
> accessible through the F1 key). Numeric data about these is also available
> when configuring the memory meter to display as text (in the Setup screen,
> F2).
>
> Why doesn't htop feature a SWAP column, like top?
>
> It is not possible to get the exact size of used swap space of a process. Top
> fakes this information by making SWAP = VIRT - RES, but that is not a good
> metric, because other stuff such as video memory counts on VIRT as well (for
> example: top says my X process is using 81M of swap, but it also reports my
> system as a whole is using only 2M of swap. Therefore, I will not add a
> similar Swap column to htop because I don't know a reliable way to get this
> information (actually, I don't think it's possible to get an exact number,
> because of shared pages).
