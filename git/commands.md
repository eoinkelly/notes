# git command overview

## Ignoring files

(edit .gitignore file) idea: git ignore "PATTERN" # appends it to the gitignore
file

### show which files are being ignored

TODO: this does not work ??? git ls-files --ignored

## Add files to staging area

- note that `add` doesn't need the `--` to separate filenames like reset does

### stage all files

git add --all git add -A

### stage 1 file

git add path/to/file

# stage 1+ files

git add path/to/dir git add path\*\*

QUESTION: what globbing works here?

# remove changes I have added to staging area

unstage all files git reset

unstage 1 files git reset -- path/to/file git rm --cached path/to/file

unstage 1+ files git reset -- path/to/\*\*

# ask git to remove files from its db but keep them in the working dir

git rm --cached \*

# List files which have been changed/added

show names of files i have cached git status --short

show names of new files i have created git status --short

## show changes to files

show changes I have changed but not cached git diff

show changes i have cached git diff --staged

show changes for new files i have created but not cached ???

```

```
