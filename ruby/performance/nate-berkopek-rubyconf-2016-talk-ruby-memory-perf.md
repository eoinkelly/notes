# Nate Berkopec Rubyconf 2016 talk

https://www.youtube.com/watch?v=kZcqyuPeDao

speaker has https://www.railsspeed.com/

Reminder: Threads share memory. Processes mostly don't.

## Ruby process memory usage graphs

The memory usage graph of most ruby processes look like an logarithm (goes up
quickly at first and then flattens out but does not go completely flat)

* first section: first 2 hours ish has memory going up quite a lot
    * because files are getting required as they are needed
        * rails tries to load everything at boot but your libs may not
    * filling up caches
        * db connection pools
        * adequate record caches
        * maybe your own application cache
    * different actions in your app require different amount of memory
* next section: surprisingly does not *fully* level out (or flatten)

## Measuring ruby process memory usage

* You MUST let the ruby process grow over many hours to see true memory usage
    * if you have higher load this will happen faster
    * in most apps you can't infer you have a leak by looking at only 1 hour
* he makes a general recommendation that 300MB per instance is a good goal for a rails app
    * this also aplies to sidekik processes
    * he says many rails apps live in 300 - 600 MB range


# His recommendations

## 1. Dial back application instance numbers until you are no longer hitting limits

* goal: discover true steady-state instance memory usage of the app
* dial back so you are not hitting limits
    * from worker killers
    * from server limits

## 2. Stop allocating so many objects at once

1. GC works on *thresholds* not timers so allocating lots of objects will tigger GC more often and cause pauses
    * there are three main thresholds
        1. no. of slots run out
            * start with 10k, multioply by 1.4 when it runs out so slots go from 10K to 14K
        1. oldmalloc
        1. malloc
1. object space (ruby callse it its "heap") fragmentation
    * ruby "object space" in memory is divided into "pages" and "slots"
        * pages contain 408 slots
        * each page is 16 Kb (depends on CPU architecture)
        * each slot is are 40 bytes
        * each slot contains an rvalue
            * each rvalue either contains the value of the object directly or a pointer to it
    * ruby cannot move objects between slots in pages
        * because of the C extension API (a C extensions can hold a pointer
          directly to any memory slot) so moving objects around would break C
          extensions and cause a segfault
            * this is why ruby cannot have a compacting GC
        * so if you allocate 1000 object and then GC 990 of them there will still be 10 scattered around in their original positions
        * ruby can only release the memory back to the OS if there are *no* objects in the page
        * you can peek at the value of the config variables that control this via
            ```
            [38] pry(main)> GC::INTERNAL_CONSTANTS
            => {:RVALUE_SIZE=>40,
            :HEAP_OBJ_LIMIT=>408, # <-- depends on your architecture
            :HEAP_BITMAP_SIZE=>56,
            :HEAP_BITMAP_PLANES=>3}
            ```
    * remember that "object space" doesn't actually contain the memory for objects themselves - just pointers to them (ignoring exceptions like integers)
        * the memory for the objects themselves is also on the OS heap
    * heap fragmentation can cause long-term slow "leaks" (actually increases in RSS usage) even in apps whose memory usage is constant
    * this is usually a small "leak" over time
    * your long-term memory usage is the peak memory usage of any one controller in the app
    * so *the key to improving rails app memory usage is to improve the memory usage of the controller which allocates the most*
    * he says scout and skylight is better than new-relic for this kind of work
    * or you can use `memory_profiler` and `oink` gems to help too

## 3. free() doesn't always give the memory back to the  OS

* malloc and free() are suggestions not commands
    * when ruby calls free() the allocator might hold on to the memory and put in a "free list" if it thinks that the ruby process will want it again soon
    * macOS has "inactive memory" which is kind of memory which could become free if needed but it isn't right now

# Solutions

* 1. diagnose problem areas and try to fix them. he does
    1. look for bad actions using an APM (new-relic, skylight etc.)
    2. dig into it with oink and memory_profiler
        * can setup before and after filters to compare things

Move the action that allocates lots into a rake task or a sidekik worker
    throwaway VMs are better than bloated VMs
Move bloated sidekiq jobs into a separate queue and run that queue on a different dyno


3. do a gemfile audit wiht `derailed`

    bundle exec derailed bundle:mem
lets you find out how much each gem costs you in memory
dependencies aren't free


4. require false for assets
if you are precompiling your assets you don't need "assets" gems in production
    e.g.
gem "sass", rquire: false
it doesn't work with all gems


## 4. jemalloc

* a different allocator than the libc one
* written by facebook

two ways to do it

1. set the LD_PRELOAD env var to load jemalloc before all other libs
2. compile ruby with `--with-jemalloc`

## 5.  use copy-on-write
use puma/unicorn with preloading
COW increases shared memory between processes
make unicorn
    1. load app
    2. then fork workers
this means that any memory used before forking can be shared between all workers


total memory usage is not the sum of RSS of each process
    this is not true because of shared memory
    this makes it look like COW isn't helping but it is
    a better metric: when I kill this work how much memory will be freed up?

COW isn't prefect but can be helpful

## 6. Use a threaded web server
basically puma and passenger only (thin has threaded mode but not commonly used)
he thinks most rails apps are probably thread safe


## 7. Keep ruby and gems up to date

pull in improvements from gem authors
be on ruby 2.2+, rails 4.2+

## 8. tune malloc

look at MALLOC_ARENA_MAX setting
glibc malloc will create areans when you have a threaded app
creates arena every time it detects a contention for memory access between threads
this defaults to something like 64
arenas improve performance but they use a lot memory
by changing the value to something lik 2 you can save quite a bit of memory

there are other things you can tune in `mallopt`

## 9. tune your GC

he doesn't recommend you do it because the ruby GC has changed a lot in last few years so don't do it unless you understand how *your exact ruby version* work

