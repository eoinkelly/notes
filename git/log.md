## Finding stuff in git log

The strategy is

1. find the commit(s) you want with `git log`
1. use `git show` to see info abou those commits

```
--oneline   # concise output
--          # end of git log options, start of file paths

# see the commit history of a single file
git log -- relative/path/to/file.ext

# find commits where a given string was added/removed from the code
# note: also finds when this string was removed!
git log -S search_string

# find a particular string in log messages
git log --grep -E -i 'some(thing)'
# -E = use extended regexps
# -i = case insensitive


git log --stat # see stats on num insertions/deletions for each file
git log -20 # see 20 log entries
git log -p # see a patch (the diff) of each commit
```

### How to see the diff history of a single file?

* You can do this by passing a file path to git log e.g.

```
# include -p to show patch for each commit
git log -p path/to/file.ext
```

* Option:
    * `gitk path/to/file.ext &`
    * `gitk --follow path/to/file.ext &`
        * follows past file renames
    * ++ works great
    * -- requires gui and gitk to be installed


### --stat

Terminology


* A "deletion" is the removal of stuff from a file
    * can be removal of a whole line or just part of one
* An "insertion" is the addtion of content to a file
    * can be part of a line or a whole line


```sh
# format:
# path/to/file.ext | {number insertions + num deletions} {whether there were insertions, deletions or both}L*L*
```
