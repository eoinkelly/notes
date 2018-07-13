## Memcached

* started in Perl in 2003, rewritten in C
* focuses on stability, not adding new features
* allegedly easier to scale horizontally because it is simpler
* keys must be strings
* keys limited to 250 bytes, values limited to 1MB, not binary safe
* data is opaque to memcached - it cannot manipulate it
* uses 3rd part clustering
* memcached works out of the box for LRU caching
* there is no official way to list all keys
    * you can send "private API" commands to the server to enumerate things - this API has no stability guarantees
* if there is a pool of memcached servers, the **client** decided which server to write its data to based on a hashing algorithm.
    * clients will write their data to **one** server in the pool only
    * no attempt is made to keep each server in sync
* it is recommended that if a cache miss happens that you don't try to failover to another server becasue if the first server comes back, the same data (of potentially different ages) will be on two servers.
* memcached servers are unaware of each other (no broadcasting, syncing or replication)
* run the server with the `-vv` flag to see log of each key read/write and other commands on stdout (running with `-v` doesn't seem to do much)

## Configuring a client

* all clients must list servers in the same order
* all clients must use the same string as hostname for each server
    * don't use "localhost" as the host for the server which is on the same box as that memcached instance because that will cause the client to make differnet hashing choices vs the choices made by other servers

## How to see what memcached options are in use

From the terminal:

```bash
# Check that memcached is running and how much memory is assigned to it (the -m
# parameter shows assigned memory)
deploy@safeplus-rails-server$ ps aux | grep memcached

# the output should include a line similar to:
memcache  4342  0.0  0.0 416308  2388 ?        Ssl  May17   0:16 /usr/bin/memcached -m 64 -U 11211 -p 11211 -u memcache -l 127.0.0.1 -c 1024 -I 1m -v
```

## Debugging

The most useful way to debug is to start the server with the `-vv` parameter and observer stdout (if you can do this easily)

Connect to the server

```
# connect to the memcached instance
nc localhost 11211

# Protocol docs:
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
# The most interesting lines in the output are:
#
#   bytes X # num bytes in use for storage
#   curr_items X # num items currently stored
#

# after connecting to the server ...
stats items

# Example output:
#
# * the first number after 'items:' is the slab number
#
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
# You must provide a <max-num-keys-to-retrieve> parameter
stats cachedump 1 100

# Example output:
ITEM foo [5 b; 0 s]
END
```


Shortcuts

```bash
# These snippets allow you to automate getting information out of the 'stats'
# command

# see how many bytes used for storage
echo "stats" | nc -w 1 localhost 11211 | awk '$2 == "bytes" { print $2" "$3 }'

# see how many items are currently in the cache
echo "stats" | nc -w 1 localhost 11211 | awk '$2 == "curr_items" { print $2" "$3 }'
```

Any arguments to the 'stats' command are considered private API to memcached and could change at any time. They are not documented in the memcached protocol. `stats cachedump` is for debugging, not intended for production use.

As well as the raw way there are some tools to do it:

* `memcached-tool`
* `memcdump`

## Debugging from Rails

In rails console:

```ruby
# get a bunch of stats about the memcached server
rails-console> Rails.cache.instance_variable_get("@data").stats["localhost:11211"]

# get number of bytes in cache
rails-console> Rails.cache.instance_variable_get("@data").stats["localhost:11211"]["bytes"]

# get number of items in cache
rails-console> Rails.cache.instance_variable_get("@data").stats["localhost:11211"]["curr_items"]
```

