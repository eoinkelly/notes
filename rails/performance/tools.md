# Performance tools for Ruby and Rails

1. gperftools
    * https://gperftools.github.io/gperftools/cpuprofile.html
2. stackprof gem
    * https://github.com/tmm3/stackprof
4. ruby-prof gem
    * https://github.com/ruby-prof/ruby-prof
5. derailed_benchmarks gem
    * https://github.com/schneems/derailed_benchmarks
6. benchmark/ips gem
    * https://github.com/evanphx/benchmark-ips
1. memory_profiler gem
    * https://github.com/SamSaffron/memory_profiler
1. whence gem
    * https://github.com/calebthompson/whence
    * very raw as of 2019-07-27
    * allows you to visualise all of the call stack paths that got to a particular line of code in a tree

Command line load testing

1. siege
1. wrk
1. ab

Appliation performance monitoring solutions for Rails apps:

1. New Relic
    * samples a % of requests - it does not run on every request
    * agent takes up non-trivial amount of RAM (30-50 MB range)
1. Skylight
    * does not sample a % of requests, every request is profiled
    * focuses on the 95th percentile
    * asserts that web perf graphs are typically log-normal not normal
    * much smaller agent
1. Scout
1. Appneta

## T-test

* https://en.wikipedia.org/wiki/Student%27s_t-test
* William Seally Gosset published under pseudonym _Student_, hence the name _T-test_
* It is a test of statistical significance
* 0.05 is a typical good number

TODO: I really don't understand this
