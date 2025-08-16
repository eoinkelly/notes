# git clone

- remote git repos are usually _bare_ repositories i.e. they don't have a
  working dir
    - by convention bare repo names end in `.git`
    - `git clone --bar REPO_PATH_OR_URL` to create a bare repo

git can use 4 protocols to access repos

1. local
    - used for files on local filesystem or things which look like a local
      filesytem e.g. NFS
    ```
    git clone path/to/existing/project project_name
    git clone file://absolute/path/to/existing/project project_name
    ```
1. http There are two versions of this protocol 1. smart HTTP protocol (newer) _
   works similarly to the git or ssh protocols 1. dumb HTTP protocol (original)
   _ expects the git repo to be exposed as files on the web server - it does a
   series of GET requests to get each file it wants _ ++ very easy to setup -
   just put a git repo under a HTTP server root and setup on post-commit hook on
   the server _ -- slow
1. ssh
    - `git clone ssh://user@server:/path/to/project`
    - or shorthand `git clone user@server:/path/to/project` which assumes SSH
      protocol
    - or shorthand `git clone server:/path/to/project` which assumes SSH
      protocol and currently logged in username
    - -- you cannot do anonymous access with SSH - even read-only access needs a
      user
1. git
    - special daemon that comes with git
    - listens on 9418
    - works similar to SSH but with ABSOLUTELY NO AUTHENTICATION
    - ++ fast
    - -- no auth
    - -- requires special firewall rules on most corporate networks
