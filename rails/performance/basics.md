## Ball park numbers

* 100-200ms response time is good for a Rails app

## Terminology

Benchmark

* A _synthetic_ measurment of the resources _or_ time consumed by a _defined procedure_
* _defined procedure_ e.g.
    * a single controller
    * a single function
* Examples
    * itertions per sec
    * num allocations

Profile

* A step by step accounting of the _relative_ consumption of resources by a procedures many subroutines
* generates a lot of numbers, not just one number
* _relative_ => most profilers measure relative to the execution of the whole operation
* _resources_
    * memory
    * time


## Performance improvement workflow

1. Read produciton metrics
1. Profile to find hotspots
1. Create benchmark
1. Iterate
1. Deploy


## Tools:

Recommended by Nate Berkopec:

1. New Relic
1. Skylight
1. Scout

## Littles law

* Originally from retail world
* is a way of modelling how many things are in a system (people in a shop, requests in a server) at any one time

The formula:

```
time-taken-to-process-each-request-in-secs * request-arrivial-rate-per-sec = num-requests-in-the-system-at-any-one-time
```

## Misc

`PumaStatsLogger::Middleware` seems good for seeing request queue depth in Puma
* the single page rails app is interesting



## Accurately talking about leaks in Ruby

There are two kinds of "leak"

1. Object space leak
    * e.g. I keep appending to an array every time I get a request
    * I/ruby still has a reference to the memory - it's not "lost" per-se
    * I keep allocating ruby objects and not allowing the GC to clean them up
    * can be caused by ruby object page fragentation - a page of ruby object refs is mostly empty but can't be deleted because there is still a few objects
2. Memory leak
    * Usually from C code via malloc etc.
    * I allocated memory but then my pointer to it went out of scope so I no longer have a reference to it and I forgot to call `free`
    * Allocating memory on the process heap

## The math of performance optimisation

### Little's law

    num-app-instances-required = avg-web-request-arrival-rate * avg-response-time-of-app

Examples:

    20 reqs/sec arriving * 0.5 sec response time = 10 app instances required
    20 reqs/sec arriving * 0.25 sec response time = 5 app instances required

    60 reqs/min = 1 req/sec
    1 reqs/sec arrive * 0.25 sec avg response time = 0.25 app instances

    4 reqs/sec arrive * 0.25 sec avg response time = 1 app instances
    2 reqs/sec arrive * 0.5 sec avg response time = 1 app instances
    5 reqs/sec arrive * 0.2 sec avg response time = 1 app instances


So `1/avg-response-time-in-sec` is the number of reqs/sec the app instance can handle

200ms response time seems common target for Rails apps
    => that 5 req/sec (300 req/min) is approx what one rails worker can handle


Littles law is based on _averages_ - it's good for ballparking but be careful!

Things which cause Little's law to not hold

* if application instances block each other in any way e.g. reading data off a socket
* if your application response times vary a lot from your average response time

95th percentile response times should be within a 4:1 ratio of average response time

You can maybe get better results out of little's law by using 95th percntile response times instead of average response times


You probably shouldn't consider puma threads as a new "instance" when evaluating littles law

### Universal scalability law

* corresponds to the core idea in _Mythical man month_
* the idea is that queue depths do not scale linearly with resource utilization - in fact they scale very dramatically (J curve)

Lots of these laws assume your system is in a _steady state_ - be careful because they won't apply if the system is not e.g.

* adding/removing people on a team
* adding/removing servers or other resources


Named times:

* time to coherence - the time the system takes to get the new resource folded in and working

## Power-law distributions

* The Pareto Principle
    * the idea that for most things, 80% of the effect comes from just 20% of the causes.
    * the _pareto distribution_ also called _power law distribution_
    * 80% of the wealth comes from 20% of the people in the economy
    * the actual ratio isn't usually 80/20, sometimes it is more severe e.g. 90/10, 99/1
    * contrast with the _bell curve/normal distribution_
* Benford's law
    * numbers tend to follow a logarithmic distribution
* Zipf's law
    * how common words are in the texts of a language follows a probabibilty distribution
    * the distribution is discrete because words are naturally discrete
    * if you were to smooth out the zipf distribution you would get the pareto distribution

> It doesn't matter what area you're working in - if you're applying equal
> effort to all areas, you are wasting your time. What the Pareto distribution
> shows us is that most of the time, our efforts would be better spent finding
> and identifying the crucial 20% that accounts for 80% of the output.


## Malloc is slow

* In any programming language, malloc is slow

@schneems thinks

* num bytes allocated is more important than object count in Ruby
* % byte allocations decrease ~= % performance imporovment
* byte allocations are more stable metrics than timings (but byte allocations is just a shorthand)
