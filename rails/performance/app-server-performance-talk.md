# Rails App Server Performance talk

- source: https://www.youtube.com/watch?v=itbExaPqNAE

he has a rule of thumb for heroku

> if you are spending more on heroku per month than your RPM you are probably
> over provisioned 1000 rpm ~= $1000/month

Process

1. Determine theoretial capacity
1. Determine how much each worker/process will use
1. Choose container size and worker counts
1. Check connection limits
1. Deploy and monitor queue depths, CPU, memory, restarts, timeouts

## Step 1: Determin theoretical capacity

Littles law

    things-in-system = arrival-rate * time-spent-in-system

so on average we can say that in a ruby server

    requests-in-system = requests-per-sec * average-response-time

                   X    = 100 r/s * 200 mS
                   X    = 100 r/S * 0.2 S
                   X    = 20 req in system (on average) at any one time

    avg-num-reqs-in-system / num-workers = utilization-percentage

    20 / 3 = 6.667 # we are using ~7% of our workers at any given time

he recomends figuring out your `requests-in-system` number and multiply it by 5
as a fudge factor to account for requests not arriving evenly spaced in time

## Step 2: Choose container size

- if you need 45 processes to serve your load, how do you choose to distribute
  those across containers? do you use 1x etc.
- regular ruby applications have a memory usage graph which looks like a
  logarithm
    - initially steep
        - caches are filled e.g. AR statement cache
    - **never goes flat** - this doesn't necessarily mean you have a memory leak
        - => you need to give ruby some headroom

Antipattern: using something like _puma worker killer_ and setting the memory
amount to kill at **too low** - this means that ruby spends too much time in the
initial steep part of its logarithm graph. You see that sawtooth pattern and you
could mistake it for a memory leak but its not He recommends letting process run
for 24hrs and see how it settles before deciding what your memory usage is.

The average rails app is between 256 and 512 MB memory usage after settling down
processes should be comfy in their dyno - 80% ish, not so much usage that they
will start to swap

Heroku perf-M and perf-L dynos don't share CPU but the 1x and 2x do Aim to have
at least 3 workers per dyno because of the way heroku routing works -

if you are struggling to fit 3 workers per dyno/container can can try jemalloc -
it can give you 5%-10% memory saving

worker counts can be 3-4x the core counts

Recommend: keep thread counts in the 3-5 range thread count too high tends to
fragment memory and means you easily go over your database connection limit

Watch out for your DB connection pool limits

- you need at least 1 connection per thread
- rack-timeout can raise waiting for PG to return which can cause the conneciton
  to be lost
- if you need 500+ PG connections, use pg_bouncer

He says NewRelic doesn't do memory profiling very well

- Skylight
- Scout
- oink - free, open source thing which will log memory usage

on Heroku MALLOC_ARENA_MAX can really help if you have many threads e.g. sidekiq
workers - see https://devcenter.heroku.com/articles/tuning-glibc-memory-behavior

Don't restart your processes more than approx 6hrs

up to 16:54 in talk
