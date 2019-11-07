# HyperLogLog

http://algo.inria.fr/flajolet/Publications/FlFuGaMe07.pdf

The analysis of near-optimal cardinality estimation algorithm


Terminology

cardinality = size

multi-set = a set where we are allowed to see elements of a set multiple times


If the set is small enough, we can always get the number of items in it (its cardinality) exactly e.g.

```sql
-- find the number of unique albumid values in a "track" table

-- first the long way
select
    count(uniq_albumid)
from (
        select albumid from track group by albumid
    ) as ids(uniq_albumid)

-- now the right way
select count(distinct albumid) from track;
```

Goal: get a good estimate of how many elements are in a set when the size of the set is too big to get the exact size

* set doesn't fit in RAM
* set doesn't fit on disk
* you only get to see the stream of items as they get added to the set

Examples

* network traffic src and dst address pairs


The promise of the paper

> estimate well beyond a billion elements with a typical accuracty of 2% using only 1.5kb memory

O(m) memory complexity
std. error ~= 1.0.4/sqrt(m)
O(1) time complexity

bit pattern observables

the position of the left most 1 bit in a binary string of a fixed lenght