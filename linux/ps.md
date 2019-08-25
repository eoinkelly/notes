
f = forest
u = user oriented format

a = list all processes with a tty
ax = list all processes whether they have tty or not

ww = don't trim wide

ps auxww
ps auxewwH


PROCESS STATE CODES

Here are the different values that the s, stat and state output specifiers
(header "STAT" or "S") will display to describe the state of a process:

        D    uninterruptible sleep (usually IO)
        R    running or runnable (on run queue)
        S    interruptible sleep (waiting for an event to complete)
        T    stopped by job control signal
        t    stopped by debugger during the tracing
        W    paging (not valid since the 2.6.xx kernel)
        X    dead (should never be seen)
        Z    defunct ("zombie") process, terminated but not reaped by its
            parent

For BSD formats and when the stat keyword is used, additional characters may
be displayed:

        <    high-priority (not nice to other users)
        N    low-priority (nice to other users)
        L    has pages locked into memory (for real-time and custom IO)
        s    is a session leader
        l    is multi-threaded (using CLONE_THREAD, like NPTL pthreads do)
        +    is in the foreground process group

About 'I'
* It's not in the man page
* It means “idle”. This state was introduced very recently, in September 2017 (version 4.14 of the Linux kernel). It is used for kernel threads which use the TASK_IDLE state when idling, instead of TASK_INTERRUPTIBLE; in previous versions of the kernel, such threads were reported as TASK_UNINTERRUPTIBLE which was confusing.

https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/fs/proc/array.c#n135
