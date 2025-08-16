# Inbox

- http://gitcasts.com/

git add -u = update staging area with manually deleted files

git init git add .

git checkout - # works like the shell one (goes to previous branch)

This means that git reset <paths> is the opposite of git add <paths>.

git commit -m "blah blah"

git status git log --pretty=oneline

git branch = shows branches git show-branch git branch "new-branch-name" =
creates a new branch git branch -d "branch-name" = deletes a branch git checkout
"branch-name" = changes to the branch "branch-name" git branch -m
old_branch_name new_branch_name = renames a branch

git tag -a 1.2 -m "my tagging message" = create an annotated tag the most recent
commit with the name "1.2" git tag -a v1.2 [commit checksum] = add a tag to a
past commit identified by it's checksum (or part of it)

git commit --amend = fixes the most recent commit

git reset --hard = return to the last commited state

git status --short --branch gives concise list of current changes to the working
dir and prefixes it with the name of the branch git status -sb # shorter version
of command above

git reset HEAD {filename} = unstage a single file git checkout -- {filename} =
discard changes to {filename} (only works if {filename} is not staged)

This is a really good git workflow:
http://nvie.com/posts/a-successful-git-branching-model/

git diff --name-only <sha1> <sha2>

# .gitignore

# If a pattern ends in slash it only matches a dir, not files or symlinks

# optional ! prefix negates the pattern

# # does comment line, empty line does nothing

# if the pathname does not contain a / git treats it as a shell glob pattern and

# checks for a match against the pathname \*relative to the location of the

# .gitignore file

# A leading slash matches the beginning of the pathname

# Git

git add -u update staging area with manually deleted files git add . add all
changed files in current dir & sub dirs

git init git commit -m "blah" git status git log git log --pretty=oneline

Branching git branch shows branches git show-branch shows branches git branch
<name> creates a new branch called <name> git branch -d <name> deletes branch
<name> git branch -D <name> deletes branch <name> FIXME diff to -d? git checkout
branch <name> change to the branch <name>

git commit --amend amends the last commit (opens vim to edit the last commit
message)
