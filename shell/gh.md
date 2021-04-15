# github cli

```bash
cd path/to/repo

gh repo view # view README in terminal

gh pr status # get PR overview


gh run list # show recent CI runs
gh run watch <ID_FROM_LIST_OUTPUT> # watch a CI run

gh run view 750252423
gh run view 750252423 --log-failed
gh run view 750252423 --log

gh run view --job 2348402401 --log
gh run view 0451 --exit-status && echo "run pending or passed"

```
