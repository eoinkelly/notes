
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
