# Conclusions

* redis is a superset of features in memcached
* redis can be configured to be a cache or a key-value store but not both at the same time
    * if you need caching and background jobs and want to use redis for both you will need two redis instances

??? what kind of redis config does action-cable need?

##  Desired config for cache

* evict the least recently used items
* never "expire" items

## Desired config for work queue (or general key-value store)

* don't evict items based on age


## Redis

* the key is a data-structure
    * ++ more flexible
    * -- not as fast if your keys are just strings
* ++ many data types
* stores data in memory
* stored procedures using embedded lua
    * lua scripts are not stored in the DB - they are sent each time but there
      is a way of using a shasum to use them from cache and avoid the overhead
      - it seems sensible at first glance.
* ++ keys and values up to 512MB each, binary safe
* ++ optional and tunable data persistance (to speed up cache warm-ups after restart)
* ++ replication
* has built in clustering since 3.0
* has "sentinel" for monitoring
* has (optimistic) transactions
* has pub/sub
* can evict LRU items - see http://redis.io/topics/lru-cache but is not configured to do this by default
* Redis also supports setting timeouts to keys so that this key will be automatically removed when a given amount of time passes.
* Situations where redis is better
    * Listing all cached keys by a specific pattern, and read or delete their values. Very easy in redis, not doable ( easily ) in memcached.
    * Storing a payload more than 1mb, easy to do in redis, requires slab size tweaks in memcached, which has performance side effects of its own.
    * Easy snapshots of current cache content
    * Redis cluster is production ready as well along with language drivers,hence clustered deployment is easy too.

## Memcached

* started in Perl in 2003, rewritten in C
* focuses on stability, not adding new features
* -- less features than redis
* ++ less complex than redis
* allegedly easier to scale horizontally because it is simpler
* key => string
* keys limited to 250 bytes, values limited to 1MB, not binary safe
* -- data is opaque to memcached - it cannot manipulate it
* uses 3rd part clustering
* ?? simpler
* memcached works out of the box for LRU caching
