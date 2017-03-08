# git subtree

* Docs: https://github.com/git/git/blob/master/contrib/subtree/git-subtree.txt
* git subtree command is different from the git subtree merge strategy

> A subtree is just a subdirectory that can be
> committed to, branched, and merged along with your project in
> any way you want.


* Allows you to treat a subdir of the project as its own repo
* You can get a separate history for it - if you get the history of a subtree
  you will see only the commits that change that subtree and then only the
  changes in those commits that affect the subtree
* In order to keep commit messages making sense, create separate commits for
  anthing that changes the files within the subtree - this way the message on
  the commit will be specific to the subtree


```bash
# squash the whole history of vim-surround into a single local commit
git subtree add
  --prefix .vim/bundle/tpope-vim-surround
  https://bitbucket.org/vim-plugins-mirror/vim-surround.git
  master
  --squash


# update the code from upstream
git subtree pull
  --prefix .vim/bundle/tpope-vim-surround
  https://bitbucket.org/vim-plugins-mirror/vim-surround.git
  master --squash

# using a remote to make the above workflow easier to type

git remote add -f tpope-vim-surround https://bitbucket.org/vim-plugins-mirror/vim-surround.git

git subtree add
  --prefix .vim/bundle/tpope-vim-surround
  tpope-vim-surround
  master
  --squash

git fetch tpope-vim-surround master
git subtree pull --prefix .vim/bundle/tpope-vim-surround tpope-vim-surround master --squash
```

