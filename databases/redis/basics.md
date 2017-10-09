# redis-cli cheatsheet

```
# connect to server
$ redis-cli

# dump info about server
redis> info

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
