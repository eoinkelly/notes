Conclusion: as of apr 2015 it seems like redis is a superset of all memcached functionality

Redis
* key => data-structure
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

Memcached

* started in Perl in 2003, rewritten in C
* focuses on stability, not adding new features
* -- less features than redis
* ++ less complex than redis
* allegedely easier to scale horizontally because it is simpler
* key => string
* keys limited to 250 bytes, values limited to 1MB, not binary safe
* -- data is opaque to memcached - it cannot manipuluate it
* uses 3rd part clustering
* ?? simpler
