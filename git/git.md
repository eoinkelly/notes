# Most important points

when merging a feature branch into master always use --no-ff as we definitely
want a merge commit

```sh
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

git remote show origin # see full details of the 'origin' remote

git pull <remote>

# is exactly the same as

git fetch <remote> git merge <remote>/<current-branch>

# you can rebase changes from a remote using

git pull --rebase <remote>

# you can configure git to always rebase with a pull

git config --global branch.autosetuprebase always

# vs

git fetch origin git log --oneline master..origin/master git merge origin/master

```

git commit --amend --no-edit
# note git does not delete old commit - it just reates new one and points HEAD at it
# does it make the old commit unreachable?

git log -p # show commits and patches - very useful

git help <ALIAS_NAME> # shows info on alias


# uncommit: undo last commit - keep its changes in the working dir
git reset --soft HEAD^


:cquit in vim exits with non-zero which causes git to notice the failure and not create the commit
```
