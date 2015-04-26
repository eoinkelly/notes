# Most important points

when merging a feature branch into master always use --no-ff as we definitely want a merge commit


A "deletion" is the removal of stuff from a file
    * can be removal of a whole line or just part of one
An "insertion" is the addtion of content to a file
    * can be part of a line or a whole line


```sh
# format:
# path/to/file.ext | {number insertions + num deletions} {whether there were insertions, deletions or both}L*L*

git log --stat # see stats on num insertions/deletions for each file
git log -20 # see 20 log entries
git log -p # see a patch (the diff) of each commit


git commit --amend
# * use the current contents of the staging area for the commit
# * replaces the previous commit
# * the commit that was replaced _can_ be recovered (presumably from the reflog)


git reset
# * works with the staging area only by default (--hard will also change working area)

git checkout
# is for updating the working tree

git checkout -- path/to/file.ext
# checkout a particular file from HEAD

# format: git checkout [commit-ref] -- [filename]

git checkout HEAD^ -- path/to/foo # get file from first parent of HEAD
git checkout HEAD^1 -- path/to/foo # same as above
git checkout HEAD^2 -- path/to/foo # get file from second parent of HEAD

git checkout HEAD~1 -- path/to/foo # get file from first ancestor of HEAD
git checkout HEAD~2 -- path/to/foo # get file from second ancestor of HEAD
git checkout HEAD~3 -- path/to/foo # get file from third ancestor of HEAD

git checkout abcdef123 -- path/to/foo # checkout from a particular commit

git checkout ek/some-work -- path/to/foo # checkout from a particular branch
```

a `<tree-ish>` is a commit, tag, tree

The difference between ~ and ^

master~3 follow the "first parent" relationships 3 levels back
master^3 follow the "second parent" relationships 3 levels back

git remote show origin # see full details of the 'origin' remote

git pull <remote>
# is exactly the same as
git fetch <remote>
git merge <remote>/<current-branch>

# you can rebase changes from a remote using
git pull --rebase <remote>

# you can configure git to always rebase with a pull
git config --global branch.autosetuprebase always
# vs
git fetch origin
git log --oneline master..origin/master
git merge origin/master


The golden rule of git rebase is to never use it on public branches.

Things rebase is good for

* making sure each commit on your private branch is meaningful

git merge-base my-feature master # find the original base of my-feature branch

> Rebases are how changes should pass from the top of hierarchy downwards and merges are how they flow back upwards.
