# Ruby performance optimization

* caching and scaling work but have limits:
    * scaling costs money
    * eventually your cache keys are so complex that only a very small % of  your users will hit it
        * when not enough users hit your cache the cost of caching starts to approach the cost of just rendering it in the first place

Code smell: cache keys that are too granular

## Reasons why ruby code is slow

* extra memory allocation which triggers GC (<-- usuall the main culprit)
* raw/algorithmic complexity of the code
* data structure copying
* context copying
* memory heavy iterators
* iterator unsafe functions

Getting statistically correct measurements

1. run test multiple times
2. statistically post-process the results to eliminate factors such as
    * power management on modern CPUs
    * load from other processes on the box you are running the test on

This is addressed again in chap 7

A history of Ruby GC
* Originally ruby had a slow implementation of a stop-the-world mark and sweep GC
* Since 2.1 it has a restricted generational GCo

Interally ruby VMs are not that different from 1.9 onwards but the difference in GC performance can make a huge difference to real world perf

Reasons to not just `GC.disable`

* Ruby's peak memory consumption will can be huge
* high memory consumption can make OS start swapping


The 80/20 rule of ruby perf improvement

80% of the improvement comes from optimizing memory usage, the remaining 20% is from everything else

Implications:

* memory optimization is fairly easy to do as you are coding
* optimize memory first!
* ruby 2.1 gives you better default memory usage but isn't perfect
* a memory optimized program has roughly the same performance on any ruby version since 1.9


```ruby
# print out current ruby memory usage (mac & linux)
puts "%d MB" % (`ps -o rss= -p#{Process.pid}`.to_i / 1024)
```

The "performance mindset"

1. Is ruby suited for this task at all?
1. How much memory will my code use? The less memory your code uses the less work the GC has to do.
    * Can I avoid intermediate objects?
1. What is the algorithmic/raw preformance of the code?
    * Once you are happy that memory is being used optimally you should look at the algorithmic perf.

Author recommends considering the above _in order_ i.e. memory usage before algorithmic complexity

END CHAP 1

## Chapter 2

Task: measure 1) no. of GC calls and 2) time taken by GC for a piece of code

* `GC.stat` will tell us no. of GC calls
* To get time for GC we have to run same code with and without GC
* Aside: `GC::Profiler` (since 1.9.2) introduces a _lot_ of memory and CPU overhead
    * => is good when you only care about relative values but not good for
      measuring the wall clock time required by GC

