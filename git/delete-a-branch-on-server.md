
git push origin --delete my-branch
# or
git push origin :my-branch


* deleting the branch name just removes the ref pointer - it does not delete any trees/blobs/commits
    * those objects might be deleted on the next `git gc` if they are no longer reachable
