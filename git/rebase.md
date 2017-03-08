# git rebase

* The golden rule of git rebase is to never use it on public branches.

Things rebase is good for

* making sure each commit on your private branch is meaningful

```
# when on your feature branch do
git rebase -i master

# find the original base of my-feature branch
git merge-base my-feature master
# ^^^ is not usually necessary

```

> Rebases are how changes should pass from the top of hierarchy downwards and
> merges are how they flow back upwards.

