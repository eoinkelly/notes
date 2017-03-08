# when you are merging branch-a and branch-b and you get a conflict

1. open the file in $EDITOR and resolve manually
1. use a visual merge tool via `git mergetool path/to/conflicting.file.ext`
1. choose a version of the file from one of the branches
1. abort the merge

```
# 1. checkout the version of the file from branch-a
git checkout branch-a -- path/to/conflicting/file.ext

# 2. stage the new version of the file (thereby marking the conflict as resolved)
git add path/to/conflicting/file.ext

# 3. abandon the whole thing (warning: if there were
#    uncommited changes in the working dir at the start of
#    the merge this will throw them away)
git merge --abort
```

