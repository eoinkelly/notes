# git merge

git merge "replays" the commits onto the working dir one at a time => it is only
ever merging one commit with the current state of the working dir => eventually
the working dir has the changes from all the commits, then git automatically
makes a new commit with those changes => that new commit has links to two
"parent" commit objects

```
git merge --ff-only some-feature
```

how to merge and use the remotes version whenever there is a conflict

git merge -X theirs some-feature

When git does a normal (non fast-forward) merge

git checkout master git merge some-feature

1. build a new tree object
    - merge the trees that correspond to the latest commits on master and
      some-feature
    - take the most recent version of each blob which has changed
    - where a single file has different blobs in each branch
        1. attempt to merge the contents of both files into a new blob
        1. tell the user to resolve it if we fail
1. create a new commit object
    - has a parent pointer to whatever commit refs/heads/master currently points
      at
    - has a parent pointer to whatever commit refs/heads/some-feature currently
      points at
    - points to the new top-level tree object git built in previous step

## A better way of working with git

before you submit PR

```
git checkout your-feature-branch
# you add code, maybe have WIP commits

# replay your nice commits onto master (so we know a --ff-only merge will work)
git rebase master

# clean up your commits now that *all* your changes are commits on top of where
# master is
git rebase -i master

# PR review happens

# do the -ff-only merge
git checkout master
git merge --ff-only your-feature-branch
```

git branch --merged # shows the branches which have been merged into the
_current_ branch git branch --no-merged # shows the branches which have been NOT
been merged into the _current_ branch

## Strategies

- some strategies take their own options (suually
- you can specify a custom strategy to `git merge` and `git pull` via the `-s`
  argument

Available strategies

- recursive (default for one branch)
    - can only resolve two heads
    - uses a 3-way merge algorithm
    - is the default merge strategy when pulling or merging one branch
    - has options (passed via `-X option_name`)
        - ours
            - resolve conflicts by favouring our code
            - easily confused with the "ours" merge strategy but is **not** the
              same !
        - theirs
            - opposite of ours option
        - patience
            - uses extra time to avoid merge conflicts
        - ignore-space-change
        - ignore-space-at-eol
        - ignore-all-space
        - diff-algorithm = patience|minimal|histogram|myers
        - there are other options too (see git-merge man page)
- resolve
    - considered safe and fast
    - can only resolve two heads
    - uses a 3-way merge algorithm
- ours
    - does not look at other trree history at all
    - discards everything the other tree did
    - declares our history contains **all** that happened
    - -- seems kind of dangerous and not super useful for general usage
    - might be useful to mark an old branch as merged but not accept any actual
      changes from it
- ocotpus

```
# recursive strategy is default
git merge X ours # merge and favour our (current local branch) code in conflict
git pull -X theirs # merge and favour their (other branch) code in conflict
```
