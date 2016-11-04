
redis-cli cheatsheet

```
$ redis-cli

keys * # list all keys (WARNING: careful if you have lots of data or care about server staying responsive)

get "keyname"
get keyname # notice keyname does not have to be quoted
set keyname "some value"
```
