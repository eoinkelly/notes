# Capistrano

* extends rake with commands for running on servers
* only works with git
* auto loads rake tasks from `lib/capistrano/tasks`
* can symlink secret config files from your app dir to a secrets dir - this keeps the secrets out of your git repo.

You define servers and roles in `config/deploy/<environment>.rb`

* roles
    * groups tasks into "roles"
    * you match 1+ "servers" to each role
    * typical rails apps have 3 roles: web, app, db

* servers
    * the defn of servers seems to be optional
    * ? can get enough info from role???

`config/deploy.rb`
    * put config common to all environments here

## how to let cap get at the git repo

option 1: ssh agent forwarding i.e. use our own ssh key to auth ourselves _from_ the server _to_ the git repo

```
#
ssh -A deploy@one-of-my-servers.com 'git ls-remote git@github.com:rabid/repo.git'
```
The above is the check that cap does internally to make sure this will work. You might have to add the git host to the lsit of known hosts on the server


option 2: HTTP auth + HTTPS can be prompted for a username and password or use Oauth token

## how to let cap ssh onto our servers

It uses ssh so provided you can ssh in to the box as your `deploy` user then cap can too.

Note: none of the default cap recipes expect sudo to be available
    passwordless sudo is probably a bad idea

You also need to make sure that the user you ssh in as has sufficient permissions to setup the app.

## cap command line

```
cap -T # show most recipe names and descriptions (like rake does)
cap <environment> <recipe> # to invoke a recipe
cap production git:check # super handy, not mentioned in cap -T ???
```

In a rails project you get a heap of built-in recipes (not sure whether they are raw cap or part of cap-rails)

```
cap deploy                         # Deploy a new release
cap deploy:check                   # Check required files and directories exist
cap deploy:check:directories       # Check shared and release directories exist
cap deploy:check:linked_dirs       # Check directories to be linked exist in shared
cap deploy:check:linked_files      # Check files to be linked exist in shared
cap deploy:check:make_linked_dirs  # Check directories of files to be linked exist in shared
cap deploy:cleanup                 # Clean up old releases
cap deploy:cleanup_rollback        # Remove and archive rolled-back release
cap deploy:finished                # Finished
cap deploy:finishing               # Finish the deployment, clean up server(s)
cap deploy:finishing_rollback      # Finish the rollback, clean up server(s)
cap deploy:log_revision            # Log details of the deploy
cap deploy:published               # Published
cap deploy:publishing              # Publish the release
cap deploy:revert_release          # Revert to previous release timestamp
cap deploy:reverted                # Reverted
cap deploy:reverting               # Revert server(s) to previous release
cap deploy:rollback                # Rollback to previous release
cap deploy:set_current_revision    # Place a REVISION file with the current revision SHA in the current release path
cap deploy:started                 # Started
cap deploy:starting                # Start a deployment, make sure server(s) ready
cap deploy:symlink:linked_dirs     # Symlink linked directories
cap deploy:symlink:linked_files    # Symlink linked files
cap deploy:symlink:release         # Symlink release to current
cap deploy:symlink:shared          # Symlink files and directories from shared to release
cap deploy:updated                 # Updated
cap deploy:updating                # Update server(s) by setting up a new release
cap install                        # Install Capistrano, cap install STAGES=staging,production
```


## capistrano-rails

* adds support for

1. asset pipeline
2. migrations


1. install capistrano
1. `cap install` from project root dir
1. move secrets out of git repo and add the files to the gitignore


# Where do dsl methods come from

* Rake
    * desc
    * task
* SSHKit
    * on
    * roles
    * test
    * info
    * error

## Aside: secrets in a rails app

database.yml

is our deploy user "locked" via `passwd -l deploy`? should it be? waht does "locked" mean?


# Flows

cap v3 provides 2 flows

1. deploy flow
2. rollback flow

cap production deploy

deploy:starting    - start a deployment, make sure everything is ready
deploy:started     - started hook (for custom tasks)
deploy:updating    - update server(s) with a new release
deploy:updated     - updated hook
deploy:publishing  - publish the new release
deploy:published   - published hook
deploy:finishing   - finish the deployment, clean up everything
deploy:finished    - finished hook

cap production deploy:rollback

deploy:starting
deploy:started
deploy:reverting           - revert server(s) to previous release
deploy:reverted            - reverted hook
deploy:publishing
deploy:published
deploy:finishing_rollback  - finish the rollback, clean up everything
deploy:finished


notice that both flows are book-ended by the same tasks
