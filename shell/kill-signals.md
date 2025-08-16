# Signals

- `man 7 signal` on linux for help

```plain
# Most important signals
0    signal 0 will not actually in fact send anything to your process's PID,
     but will check whether you have permissions to do so.
1    HUP (hang up)
2    INT (interrupt) CTRL-c
3    QUIT (quit)     CTRL-\
6    ABRT (abort)
9    KILL (non-catchable, non-ignorable kill)
10   USR1 (user defined)
12   USR2 (user defined)
14   ALRM (alarm clock)
15   TERM (software termination signal)
18   CONT (Continue if stopped)
19   STOP (cannot be caught, blocked or ignored, Stop process)
20   TSTP (e.g. Sidekiq uses this)
21   TTIN (e.g. Sidekiq uses this)
```

- every signal has a default action associated
- Default actions:
    1. terminate the process (`Term` in man page)
    1. dump core (`Core` in man page)
    1. ignore (`Ign` in man page)
    1. stop the process (`Stop` in man page)
    1. continue the process (`Cont` in man page)
- by default linux will not actually dump cores unless you tell it to e.g. in
  bash `ulimit -c unlimited`
- a process (whether script or binary) can trap signals and do something custom
  with them
- A PID of `-1` indicates all processes except the `kill` process and init
- Keyboard bound signals
    - `CTRL-c` sends INT
    - `CTRL-d` sends EOF which is not actually a signal
    - `CTRL-\` sends QUIT
- the kill command
    - `kill` can be either a shell built-in (`zsh` does this) or a binary in
      `/bin/kill`
    - the default signal sent by `kill` is `TERM`
- the `SIGRT*` signals are real-time (if your OS supports them, linux does since
  2.2)

## Ways of sending signals

```plain
# kill -<SIGNALNUMBER> <PID>
kill -1 999

# kill -s <SIGNALNAME> <PID>
kill -s HUP 999

# stty -e shows you which keyboard shortcuts are bound to which signals
$ stty -e
speed 38400 baud; 51 rows; 88 columns;
lflags: icanon isig iexten echo echoe echok echoke -echonl echoctl
	-echoprt -altwerase -noflsh -tostop -flusho pendin -nokerninfo
	-extproc
iflags: -istrip icrnl -inlcr -igncr -ixon ixoff ixany imaxbel iutf8
	-ignbrk brkint -inpck -ignpar -parmrk
oflags: opost onlcr -oxtabs -onocr -onlret
cflags: cread cs8 -parenb -parodd hupcl -clocal -cstopb -crtscts -dsrflow
	-dtrflow -mdmbuf
discard dsusp   eof     eol     eol2    erase   intr    kill    lnext
^O      ^Y      ^D      <undef> <undef> ^?      ^C      ^U      ^V
min     quit    reprint start   status  stop    susp    time    werase
1       ^\      ^R      ^Q      ^T      ^S      ^Z      0       ^W
```

# Full lists of signals

```plain
# maxOS
kill -l
HUP INT QUIT ILL TRAP ABRT EMT FPE KILL BUS SEGV SYS PIPE ALRM TERM URG STOP TSTP CONT CHLD TTIN TTOU IO XCPU XFSZ VTALRM PROF WINCH INFO USR1 USR2

# linux
kill -l
 1) SIGHUP	 2) SIGINT	 3) SIGQUIT	 4) SIGILL	 5) SIGTRAP
 6) SIGABRT	 7) SIGBUS	 8) SIGFPE	 9) SIGKILL	10) SIGUSR1
11) SIGSEGV	12) SIGUSR2	13) SIGPIPE	14) SIGALRM	15) SIGTERM
16) SIGSTKFLT	17) SIGCHLD	18) SIGCONT	19) SIGSTOP	20) SIGTSTP
21) SIGTTIN	22) SIGTTOU	23) SIGURG	24) SIGXCPU	25) SIGXFSZ
26) SIGVTALRM	27) SIGPROF	28) SIGWINCH	29) SIGIO	30) SIGPWR
31) SIGSYS	34) SIGRTMIN	35) SIGRTMIN+1	36) SIGRTMIN+2	37) SIGRTMIN+3
38) SIGRTMIN+4	39) SIGRTMIN+5	40) SIGRTMIN+6	41) SIGRTMIN+7	42) SIGRTMIN+8
43) SIGRTMIN+9	44) SIGRTMIN+10	45) SIGRTMIN+11	46) SIGRTMIN+12	47) SIGRTMIN+13
48) SIGRTMIN+14	49) SIGRTMIN+15	50) SIGRTMAX-14	51) SIGRTMAX-13	52) SIGRTMAX-12
53) SIGRTMAX-11	54) SIGRTMAX-10	55) SIGRTMAX-9	56) SIGRTMAX-8	57) SIGRTMAX-7
58) SIGRTMAX-6	59) SIGRTMAX-5	60) SIGRTMAX-4	61) SIGRTMAX-3	62) SIGRTMAX-2
63) SIGRTMAX-1	64) SIGRTMAX


# linux: /bin/kill --table
 1 HUP
 2 INT
 3 QUIT
 4 ILL
 5 TRAP
 6 ABRT
 7 BUS
 8 FPE
 9 KILL
10 USR1
11 SEGV
12 USR2
13 PIPE
14 ALRM
15 TERM
16 STKFLT
17 CHLD
18 CONT
19 STOP
20 TSTP
21 TTIN
22 TTOU
23 URG
24 XCPU
25 XFSZ
26 VTALRM
27 PROF
28 WINCH
29 POLL
30 PWR
31 SYS
```

From the `man 7 signal` man page

```plain
Standard signals

Linux supports the standard signals listed below.  Several signal  numbers  are  architecture-dependent,  as
indicated  in  the  "Value" column.  (Where three values are given, the first one is usually valid for alpha
and sparc, the middle one for x86, arm, and most other architectures, and the last one  for  mips.   (Values
for  parisc  are  not  shown;  see  the Linux kernel source for signal numbering on that architecture.)  A -
denotes that a signal is absent on the corresponding architecture.)

First the signals described in the original POSIX.1-1990 standard.

Signal     Value     Action   Comment
──────────────────────────────────────────────────────────────────────
SIGHUP        1       Term    Hangup detected on controlling terminal
                                or death of controlling process
SIGINT        2       Term    Interrupt from keyboard
SIGQUIT       3       Core    Quit from keyboard
SIGILL        4       Core    Illegal Instruction
SIGABRT       6       Core    Abort signal from abort(3)
SIGFPE        8       Core    Floating point exception
SIGKILL       9       Term    Kill signal
SIGSEGV      11       Core    Invalid memory reference
SIGPIPE      13       Term    Broken pipe: write to pipe with no
                                readers
SIGALRM      14       Term    Timer signal from alarm(2)
SIGTERM      15       Term    Termination signal
SIGUSR1   30,10,16    Term    User-defined signal 1
SIGUSR2   31,12,17    Term    User-defined signal 2
SIGCHLD   20,17,18    Ign     Child stopped or terminated

SIGCONT   19,18,25    Cont    Continue if stopped
SIGSTOP   17,19,23    Stop    Stop process
SIGTSTP   18,20,24    Stop    Stop typed at terminal
SIGTTIN   21,21,26    Stop    Terminal input for background process
SIGTTOU   22,22,27    Stop    Terminal output for background process

The signals SIGKILL and SIGSTOP cannot be caught, blocked, or ignored.

Next the signals not in the POSIX.1-1990 standard but described in SUSv2 and POSIX.1-2001.

Signal       Value     Action   Comment
────────────────────────────────────────────────────────────────────
SIGBUS      10,7,10     Core    Bus error (bad memory access)
SIGPOLL                 Term    Pollable event (Sys V).
                                Synonym for SIGIO
SIGPROF     27,27,29    Term    Profiling timer expired
SIGSYS      12,31,12    Core    Bad argument to routine (SVr4)
SIGTRAP        5        Core    Trace/breakpoint trap
SIGURG      16,23,21    Ign     Urgent condition on socket (4.2BSD)
SIGVTALRM   26,26,28    Term    Virtual alarm clock (4.2BSD)
SIGXCPU     24,24,30    Core    CPU time limit exceeded (4.2BSD)
SIGXFSZ     25,25,31    Core    File size limit exceeded (4.2BSD)

Up to and including Linux 2.2, the default behavior for SIGSYS,  SIGXCPU,  SIGXFSZ,  and  (on  architectures
other  than  SPARC and MIPS) SIGBUS was to terminate the process (without a core dump).  (On some other UNIX
systems the default action for SIGXCPU and SIGXFSZ is to terminate the process without a core dump.)   Linux
2.4 conforms to the POSIX.1-2001 requirements for these signals, terminating the process with a core dump.


Next various other signals.

Signal       Value     Action   Comment
────────────────────────────────────────────────────────────────────
SIGIOT         6        Core    IOT trap. A synonym for SIGABRT
SIGEMT       7,-,7      Term
SIGSTKFLT    -,16,-     Term    Stack fault on coprocessor (unused)
SIGIO       23,29,22    Term    I/O now possible (4.2BSD)
SIGCLD       -,-,18     Ign     A synonym for SIGCHLD
SIGPWR      29,30,19    Term    Power failure (System V)
SIGINFO      29,-,-             A synonym for SIGPWR
SIGLOST      -,-,-      Term    File lock lost (unused)
SIGWINCH    28,28,20    Ign     Window resize signal (4.3BSD, Sun)
SIGUNUSED    -,31,-     Core    Synonymous with SIGSYS

(Signal 29 is SIGINFO / SIGPWR on an alpha but SIGLOST on a sparc.)

SIGEMT  is  not  specified  in  POSIX.1-2001, but nevertheless appears on most other UNIX systems, where its
default action is typically to terminate the process with a core dump.

SIGPWR (which is not specified in POSIX.1-2001) is typically ignored by default on those other UNIX  systems
where it appears.

SIGIO (which is not specified in POSIX.1-2001) is ignored by default on several other UNIX systems.

Where defined, SIGUNUSED is synonymous with SIGSYS on most architectures.
```
