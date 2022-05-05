


# compare two directories with rsync

By default rsync compares mtime and size
adding -c tells it to compre checksums

```bash

# -a = turn on archive mode. same as: -rlgtgoD
# -v = verbose, more v's for more verbosity
# -u = skip files that  are newere on the receiver
# -n = dry run
# -c = compare checksums instead of mtime+size (slower)
# the trailing slashes matter - they tell rsync to compare the contents of these dirs, not the dirs themselves
rsync -avvcun src/ dest/
```
