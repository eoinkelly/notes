## Memcached

* started in Perl in 2003, rewritten in C
* focuses on stability, not adding new features
* allegedly easier to scale horizontally because it is simpler
* keys must be strings
* keys limited to 250 bytes, values limited to 1MB, not binary safe
* data is opaque to memcached - it cannot manipulate it
* uses 3rd part clustering
* memcached works out of the box for LRU caching
* there is no way to list all keys
* if there is a pool of memcached servers, the **client** decided which server to write its data to based on a hashing algorithm.
    * clients will write their data to **one** server in the pool only
    * no attempt is made to keep each server in sync
* it is recommended that if a cache miss happens that you don't try to failover to another server becasue if the first server comes back, the same data (of potentially differnt ages) will be on two servers.
* memcached servers are unaware of each other (no broadcasting, syncing or replication)

## Configuring a client

* all clients must list servers in the same order
* all clients must use the same string as hostname for each server - don't use "localhost" as the host for the server which is on the same box as that memcached instance because that will cause the client to make differnet hashing choices vs the choices made by other servers

## Examples
```
# connect to the memcached instance
telnet localhost 11211

# https://github.com/memcached/memcached/blob/master/doc/protocol.txt

# set <key> <flags> <expires_in_secs> <bytes_in_data_block>
# <flags> can be any int
set foo 1 0 5
hello
STORED

# get <key>
get foo
VALUE foo 1 5
hello
END


stats
# full list of stats available at https://github.com/memcached/memcached/blob/master/doc/protocol.txt
# bytes X # num bytes in use for storage
# curr_items X # num items currently stored
```

```
# see how many bytes used for storage
echo "stats" | nc -w 1 localhost 11211 | awk '$2 == "bytes" { print $2" "$3 }'
```

## How to see all items

Any arguments to the 'stats' command are considered private API to memcached and could change at any time. They are not documented in the memcached protocol. `stats cachedump` is for debugging, not intended for production use.

As well as the raw way there are some tools to do it:

* `memcached-tool`
* `memcdump`

```
# after telneting to server ...
stats items

# the first number after 'items:' is the slab number
STAT items:1:number 1
STAT items:1:age 69187
STAT items:1:evicted 0
STAT items:1:evicted_nonzero 0
STAT items:1:evicted_time 0
STAT items:1:outofmemory 0
STAT items:1:tailrepairs 0
STAT items:1:reclaimed 0
STAT items:1:expired_unfetched 0
STAT items:1:evicted_unfetched 0
STAT items:1:crawler_reclaimed 0
STAT items:1:crawler_items_checked 0
STAT items:1:lrutail_reflocked 0
END

# dump the slab to see items in it
# stats cachedump <slab-number> <max-num-keys-to-retrieve>
stats cachedump 1 100
ITEM foo [5 b; 0 s]
END
```
