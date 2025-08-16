# git object model

Git has ? types of object

1. blob
    - represents a file **contents**, not file name or anything else
    - stored in the "object store" `.git/objects`
    - name in storage is calculated by SHA1 hash of `TYPE SIZE\0CONTENTS`
    - first two chars of hash are pulled off to make dir name
1. tree
    - represents a directory (but is not the same as a directory)
    - not created when you add files to staging area
    - are created when you commit
1. commit

### working through an example

When we do these actions:

```bash
$ git init
$ echo "hello" >README.md
$ git add README.md
```

we see it create one blob in the repo:

```
.git
├── HEAD
├── config
├── hooks
│   ├── post-checkout
│   ├── post-commit
│   ├── post-merge
│   ├── post-rewrite
│   ├── pre-commit
│   └── pre-push
├── index
├── objects
│   ├── ce
│   │   └── 013625030ba8dba906f756967f9e9ca394464a
│   ├── info
│   └── pack
└── refs
    ├── heads
        └── tags
```

### Hashing

- git uses SHA-1 for hashing (same as the default output of `shasum` command)
- when storing objects within `.git/objects` it uses the first two chars of hash
  dir name and the rest as filename

git calculates the blob dir/filename by

```
# -e tells echo to interpret null byte correctly
echo -e 'blob 6\0hello' | shasum
ce013625030ba8dba906f756967f9e9ca394464a -
```

i.e. it prepends the file contents with:

1. its type `blob`
2. length 6 chars including newline
3. `\0` null byte to separate git header from file contents

Once it has the data it compresses it with zlib before storing it on disk

### git cat-file

- can dump following info on repository objects
    - contents (this is default, you have to tell git the type on cmd line too
    - type
    - size
    - pretty printed contents
- `git cat-file -p SHA1_OF_INTERESTING_OBJECT` is the most useful usage (without
  -p you have to specify the type)

```bash
# git cat-file TYPE SHA
$ git cat-file blob ce0136250
hello

# show type
git cat-file -t ce0136250
blob

# pretty print (note you don't have to specify type)
git cat-file -p  ce0136250
hello

# show size
git cat-file -s ce0136250
6
```

Now when we commit our previous example:

```bash
$ gcm -m "Add README"
[master (root-commit) 76079f1] Add README
 1 file changed, 1 insertion(+)
 create mode 100644 README.md

$ tree .git
.git
├── COMMIT_EDITMSG
├── HEAD
├── config
├── hooks
│   ├── post-checkout
│   ├── post-commit
│   ├── post-merge
│   ├── post-rewrite
│   ├── pre-commit
│   └── pre-push
├── index
├── logs
│   ├── HEAD
│   └── refs
│       └── heads
│           └── master
├── objects
│   ├── 76
│   │   └── 079f18cd7619ac5864430914b9efff3b35c5ee
│   ├── 85
│   │   └── 3694aae8816094a0d875fee7ea26278dbf5d0f
│   ├── ce
│   │   └── 013625030ba8dba906f756967f9e9ca394464a
│   ├── info
│   └── pack
└── refs
    ├── heads
    │   └── master
    └── tags

13 directories, 16 files
```

We see that git has created two more objects in the object store. These objects
are:

1. a commit object
1. a tree object

When we inspect these files

```
# first get type
git cat-file -t 76079f18
commit

# then use type to get contents
$ git cat-file -p 76079f18
tree 853694aae8816094a0d875fee7ea26278dbf5d0f
author Eoin Kelly <eoin@rabidtech.co.nz> 1487364614 +1300
committer Eoin Kelly <eoin@rabidtech.co.nz> 1487364614 +1300

Add README

$ git cat-file -t 853694aa
tree
$ git cat-file -p 853694aa
100644 blob ce013625030ba8dba906f756967f9e9ca394464a  README.md
```

When you add a file to the index git will

1. create an blob to represent the file contents in the object store

When you make a commit git will

1. create a tree to represent the change
1. create a commit file which points at the tree

### Blob objects

TODO

### Tree objects

- each line in the tree is listing data about either
    - a blob
    - another tree
- each line in the tree is of form
  `{mode-and-permissions-as-ascii}{space}{filename}{null-byte}{unknown-binary-data}`
    - in upcase video the tree seems to be ascii - whats up ???
- filenames in git are stored in the tree objects
    - => if you rename a file the only object which needs to change is the tree
      object
- tree objects cannot be empty - git will not create one if there are no entries
    - => git ignores empty dirs
- a tree object can contain any number of blobs or trees
- Use `git cat-file -p SHA1_OF_TREE_OBJECT` to inspect a raw tree object
- Use `git ls-tree SHA1_OF_TREE_OBJECT` to see another view of a tree object

### Commit objects

commits point at trees

commit objects are text file with following fields

1. tree
    - sha1 hash of **the single** tree which this commit points to
1. parent (optional)
    - sha1 hash of another commit object
    - Only one commit in your repo will not have a parent commit - this is
      called the "root" commit
1. author
    - Name, email, timestamp
1. commiter
    - Name, email, timestamp
1. commit message
    - whatever message we typed in

A commit object ...

- connects everything together - is the "hub" of the git object graph
- Use `git cat-file -p SHA1_OF_COMMIT_OBJECT` to inspect a raw commit object

## Diffs

- Git does not **store** diffs - it stores snapshots of file contents
- it calculates diffs in memory when it needs them

## Branches

- branches point at commits

## Reflog

- is local to a repository - does not get pushed & pulled
- a log of any changes to references in the repository
- gets updated anytime a command changes your working directory

git-gc will remove reflog entries older than an expire time or not reachable
from the current tip

QUESTION: waht things destory the reflog? gc?

Questions

what does .git/HEAD do? it seems to point to a ref within .git/refs

what is job of .git/index ??? is binary file

.git/logs I _think_ this is where the reflog is stored seems to be a history of
all changes I made locally to the object store

local branches are called "heads" and stored in `.git/refs/heads` because they
are things that HEAD can point at
