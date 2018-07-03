# Capistrano

### Overview

* extends the **rake DSL** with methods for running commands on remote servers (over SSH)
* In general it does
    1. connect to the given server over SSH
    1. run the command (or sequence of commands) you give it
* it auto loads any custom rake tasks you have defined in `lib/capistrano/tasks`
* it can symlink secret config files from your app dir to a secrets dir - this keeps the secrets out of your git repo.
* use cases
    * deploy apps
    * run audits on multiple machines
    * setup machines by driving things like `chef-solo`
* it supports rails really well
* it can only do deploys with git (or similar source control)

### How it organises servers

* roles
    * groups servers into "roles"
* server
    * if you only have one server you can define it directly without using a role


### Where the config lives

In your rails app
```
Capfile
# execution of the 'cap stage cmd' begins here

lib/capistrano/tasks/*.rake
# source by Capfile

config/deploy.rb
# * config common to all environments here
# * config starts here and is specialized by config/deploy/envname.rb

config/deploy/envname.rb # envname specific config here
```


### Configuring a rails app

The following roles are part of how capistrano works by default

1. `:all`
    * you cannot change this, it is automatically added as a role to all servers
    * e.g. `capistrano-bundler` runs for servers with the `:all` role by default
    * e.g. `capistrano-rbenv` runs for servers with the `:all` role by default
        * you can tweak this by changing `:rbenv_roles` attribute
1. `:app`
    * intended for _rails_ servers
1. `:db`
    * intended for hosts that run your databases (on the assumption they may not be the same as those running your rails servers)
    * the primary server in this role (which is either the first one, or the one with the `:primary` attribute set) is the only server that migrations are run on by default
1. `:web`
    * intended for your _web_ servers e.g. nginx, apache etc.
    * all servers with this role get asset precompilation

capistrano-rails has some custom settings which depend on capistrano built-in name convention roles

* `assets_roles`
    * servers in any roles in this array get asset precompiliation
    * is plural, there can be many roles which need asset precompilation
    * defaults to `[:web]`
* `migration_role`
    * defaults to `:db`
    * is singular - there is only one "migration role"
* `migration_servers`
    * Defaults to the primary server in the `migration_role` (The primary server in each group is considered to be the first unless any hosts have the primary property set)
    * can be one-many servers


### how to let cap get at the git repo

####  option 1: ssh agent forwarding

i.e. use our own ssh key to auth ourselves _from_ the server _to_ the git repo

```
ssh -A deploy@one-of-my-servers.com 'git ls-remote git@github.com:rabid/repo.git'
```

The above is the check that cap does internally to make sure this will work. You might have to add the git host to the list of known hosts on the server


    TODO: this is very vague, improve.

#### option 2: HTTP auth + HTTPS

* can be prompted for a username and password or use Oauth token

## how to let cap ssh onto our servers

It uses ssh so provided you can ssh in to the box as your `deploy` user then cap can too.

Note: none of the default cap recipes expect sudo to be available
    passwordless sudo is probably a bad idea

You also need to make sure that the user you ssh in as has sufficient permissions to setup the app.

## Running capistrano on the command line

```
cap -T # show most recipe names and descriptions (like rake does)

# General form of commands:
cap <environment> <recipe> # to invoke a recipe

cap production git:check # super handy, not mentioned in 'cap -T' for some reason
```

In a rails project you get a heap of built-in recipes (some come from
`capistrano` itself, some from `capistrano-rails` and other capistrano plugin
gems e.g. `capistrano-rbenv`, `capistrano-rake` etc.)

```plain
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

Usage

1. install capistrano
1. `cap install` from project root dir
1. move secrets out of the git repo and add the files to the gitignore (???)

# Where do DSL methods come from

* Rake
    * desc
    * task
* SSHKit
    * on
    * roles
    * test
    * info
    * error

# Flows

Capistrano v3 provides 2 flows:

1. deploy flow
2. rollback flow

```plain
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
```

notice that both flows are book-ended by the same tasks
