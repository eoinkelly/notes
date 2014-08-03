# Rails Performance

##  http://confreaks.com/videos/4107-rdrc2014-speed-up-rails-speed-up-your-code

* Typically we trade off time and memory space when improving performance
    * Implications:
        * To improve speed, we use more RAM
        * To improve RAM usage, we go slower
* There is also the possibility of finding a better algorightm but this is less
  common.

Tools

What do we measure?

We measure the performance of individual methods.
? Does it make sense to measure performance of a class?

`benchmark/ips`
* iterations per second
* provides standar deviations
* you give it a block to run as many times as it can in X seconds

* ruby has `benchmark` gem built-in.

Things to watch out for

* Noise - how much other work is your machine doing at the same time? How much
does this change between benchmarks
    * It is handy to have a standard deviation here so you can see how noisy
      your result is.
    "say the slowest was X and the fastest Y and it was mostly in the middle at Z"
    TODO refresh std dev
    * `benchmark/ips` can show you std dev


stackprof
GC.stat()
GC.stat(:total_allocated_objects) # num of objects allocated in the system since start
allocation_tracer gem
