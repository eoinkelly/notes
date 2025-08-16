# Three-way merge

git makes merge commits using three commits

1. commit at tip of one branch
1. commit at tip of the other branch
1. commit which is the common ancestor of both tips

first it finds the snapshot (trees + blobs) that correspond to each of those
commits then makes a new snapshot (trees + blobs) based on merging the trees
from those three trees.

it then creates a commit to point to the new top-level tree this commit is
special because it has two parents

=> git actually "adds code" to your repo when you do a merge

when you create a merge commit git + you are adding new code to the repo
