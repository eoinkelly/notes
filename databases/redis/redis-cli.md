# redis-cli cheatsheet

Tools

- redis-cli
    - comes with redis server
- https://redisdesktop.com/
    - paid
        - $15 from mac/microsoft app store
        - free on linux
- https://www.redsmin.com/
    - subscription but has a free tier

Installing `redis-cli`

- macOS: `brew install redis`
    - comes with the server
    - server config file location: `/usr/local/etc/redis.conf`
- Ubuntu linux: `apt install redis-tools`

what's the deail with redis "db numbers"?

ElastiCache

- as of 2020-09-30 latest Redis version is 5.0.6 (Locally I have 6.0.8)
- Configures Redis with parameter groups
    - Parameter group has 171 settings for Redis 5
    - Default parameter group is not editiable but you can create custom groups
- latency for elasticache seems to ba round 1sec - that's not great :-(

### Configuring for Sidekiq vs Rails cache

    # elasticache defaults
    lru_clock:7582386
    maxmemory_policy:volatile-lru

The Elasticache defaults are good for Rails cache but `maxmemory_policy` should
be changed for Sidekiq

Sidekiq recommends setting `maxmemory_policy noeviction`

- https://github.com/mperham/sidekiq/wiki/Using-Redis#memory
- https://redis.io/topics/lru-cache (details of various policies)

## Commands

```
# command line param equivalents of running interactively

# get configuration (set by redis.conf)
$ redis-cli config get '*'
```

```
# connect to server
$ redis-cli

# connect to a non-default host
$ redis-cli -h myredis.aws.internal

# connect to database 4 instead of 0 (the default)
$ redis-cli -n 4

# get a realtime dump of commands as the server receives them
redis> monitor

# dump runtime info about server
redis> info

# get configuration (set by redis.conf)
redis> config get *

# see num keys in currently selected DB https://redis.io/commands/dbsize
redis> dbsize

# list all keys (WARNING: careful if you have lots of data or care about server staying responsive)
redis> keys *

# get value of key
redis> get "keyname"
redis> get keyname # notice keyname does not have to be quoted

# set value of key
redis> set keyname "some value"

# exit the cli
redis> exit
```

```
# continious latency sampling
redis-cli -h my.cache.server -n 6 --latency

# sample for 5 sec
redis-cli -h my.cache.server -n 6 -n 6 --latency -i 5 --raw


# show latency history
redis-cli -h my.cache.server -n 6 --latency-history -i 5
min: 0, max: 11, avg: 1.03 (448 samples) -- 5.01 seconds range
min: 0, max: 3, avg: 1.03 (448 samples) -- 5.00 seconds range
min: 0, max: 22, avg: 1.06 (446 samples) -- 5.00 seconds range
min: 0, max: 3, avg: 0.98 (450 samples) -- 5.01 seconds range
min: 0, max: 3, avg: 1.01 (448 samples) -- 5.01 seconds range
min: 0, max: 3, avg: 1.01 (448 samples) -- 5.01 seconds range
min: 0, max: 10, avg: 1.05 (446 samples) -- 5.01 seconds range
min: 0, max: 3, avg: 1.01 (447 samples) -- 5.01 seconds range


redis-cli -h my.cache.server -n 6
```
