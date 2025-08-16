# The git staging area

When you "add files to the staging area" what really happens is

1. git creates blobs for any changed/new files in object storage aka
   `.git/objects`
    - it does not seem to create tree objects in object storage for any dirs
      which have changes
1. git adds references to the changes to the .git/index file Use
   `git ls-files -s` to inspect the index

When you "git reset" to remove files from the staging area

1. git does not remove any objects it created in object storage
1. git removes the reference from `.git/index`

## the git index

- used as part of the "staging area"

git seems to keep lots of files in `.git/index` that are not staged changes -
what is going on here?
