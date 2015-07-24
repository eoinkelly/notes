# Unix Load averages

* Load "average" over 1, 5, 15 mins - it is the _average_ no. of "jobs" in the
  "run queue"
* It is not strictly an "average" - strictly speaking the values are calculated
  by an exponential decay function but for our purposes we can consider it an
  "average"

> reading from left to right, one can examine the aging trend and/or duration
> of the particular system state. The state in question is CPU load—not to be
> confused with CPU percentage. In fact, it is precisely the CPU load that is
> measured, because load averages do not include any processes or threads
> waiting on I/O, networking, databases or anything else not demanding the CPU.
> It narrowly focuses on what is actively demanding CPU time. This differs
> greatly from the CPU percentage.

# CPU percentage

> The CPU percentage is the amount of a time interval (that is, the sampling
> interval) that the system's processes were found to be active on the CPU. If
> top reports that your program is taking 45% CPU, 45% of the samples taken by
> top found your process active on the CPU.  The rest of the time your
> application was in a wait.

> It is important to remember that a CPU is a discrete state machine. It
> really can be at only 100%, executing an instruction, or at 0%, waiting for
> something to do. There is no such thing as using 45% of a CPU. The CPU
> percentage is a function of time.

> However, it is likely that your application's rest periods include waiting to
> be dispatched on a CPU and not on external devices. That part of the wait
> percentage is then very relevant to understanding your overall CPU usage
> pattern.

> The load averages differ from CPU percentage in two significant ways: 1) load
> averages measure the trend in CPU utilization not only an instantaneous
> snapshot, as does percentage, and 2) load averages include all demand for the
> CPU not only how much was active at the time of measurement.

It is measuring processes that are marked running or uninterruptible.

* http://blog.scoutapp.com/articles/2009/07/31/understanding-load-averages
* You can think of cores as lanes of traffic on a bridge (4 core CPU ~= 4 lane bridge)

> for the purposes of sizing up the CPU load value, the total number of cores
> is what matters, regardless of how many physical processors those cores are
> spread across.

On a single CPU/core machine

* 0.0 means no jobs are going through the CPU
* 1.0 means that single CPU is exactly at capacity
* 1+ means that jobs are waiting to get on the CPU

If you have 4 cores then 4.0 would be all 4 cores exactly at capacity

Rules of thumb:

1. No. of cores = max load
2. Cores are cores
    * Roughly speaking a 4 processor of 1 core machine == 1 processor of 4 core machine
3. 0.7 per core/cpu is about the most you want in reality as it gives you some headroom
4. 1.0 per core is too high for comfort
5. 5.0 or above for core is a really serious problem
6. The 5 and 15 min averages matter more than the 1 min (which could be a short spike)

For a 2 core server:

* 1.4 is the max 5 or 15 min average you would want to see regularly
* 2.0 is the CPU at max capacity

Both `top` and `uptime` show load averages.

# Find no. of cores a system has

To find out how many cores your system has:

```
# Linux
$ cat /proc/cpuinfo

# Mac
$ sysctl hw.ncpu
$ sysctl -n hw.ncpu' # gets just number, handy to use programmatically
```
