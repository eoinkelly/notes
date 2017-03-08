# git checkout

```
# you can specify which branch to use as the starting point for the new branch
git checkout -b new-branch-name starting-branch
```

Uses of checkout

1. move the HEAD pointer to point to another branch within `.git/refs/{heads|remotes|tags}`
1. move the HEAD pointer to point direclty to a commit ("detached HEAD")
1. create a new ref within `.git/refs/heads`
    * note `git tag` is used to create a new ref within `.git/refs/tags`
1. get a version of a file from a commit
    * To run: `git checkout BRANCH_NAME -- path/to/file.ext` git does
        1. find the commit pointed at by BRANCH_NAME
        2. find the tree pointed at by that commit
        3. find the `path` tree within that tree
        4. find the `to` tree within that tree
        5. find the sha1 sum of the `file.ext` blob reference within that tree
        6. extract the blob referenced by the sha1
        7. save the data into `path/to/file.ext` in the current working dir
