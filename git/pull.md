# git pull

- is a wrapper over git fetch and git merge
- most of its options are passed through to those commands

```
$ git pull

# is basically

$ git fetch
$ git merge FETCH_HEAD
```

```
you can run `git pull --rebase` to do a rebase rather than a merge


$ git pull --ff-only # to allow only fast-forward merges

$ git pull --squash
# * fetches and leaves working tree and index as if the
#   merge happened but doesn't actually do the merge
# * it allows you to create a new commit which contains
#   all the changes from the origin
```
