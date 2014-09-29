# Chef

* Each git repo is a cookbook (a collection of recipes)
* Chef repos have a vagrantfile so can be a shared dir with a Vagrant managed VM
* Has a Thorfile which is presumably used by berkshelf

* metadata.rb
    * a collection of method calls
    * lets you declare dependencies on other cookbooks
        QUESTION: what is diff between metadata.rb depends and berksfile???

* Berksfile
    * Berksfile : cookbooks as Gemfile : gems
    * provides links to load other cookbooks

attributes/default.rb
    * sets values in a hash named "default"
    * presumably there can be other attribute files with different hashes???

templates
    /default
        /application.yml.erb
            * sets what looks like envornment vars from instance variables
            * what context is this loaded in?

recipes/default.rb
    * again presume 'default' is an environment


TODO: is the "default" the same as the "\_default" environment on the server?

## Berkshelf

* A cookbook manager
* provides generators for making new cookbook and initializing a dir as a cookbook
* similar ish to bundler for rails apps (if bundler included some functions of rails cli tool)
* manages dependencies on other cookbooks


```
berks help      # get help
bers install    # install cookbooks mentioned in Berksfile
berks init      # setup current dir as a cookbook dir
berks cookbook mycb
```


commands I ran for kete:
```
berks cookbook chef-kete

# upload to the configured chef server
# berks upload
```

cookbooks are installed in `~/.berkshelf` (change via BERKSHELF_PATH env var)
without berkshelf cookbooks are usually in /site-cookbooks within the chef repo

berkshelf is waht integrates with vagrant

```
# one time vagrant plugin install
vagrant plugin install vagrant-berkshelf

vagrant provision
# runs any "provisioners" you have setup in the Vagrantfile
```

berkshelf provides 2 provisioners

1. chef-solo
2. chef-client


QUESTION: the Berksfile says what should be uploaded to the chef server

## metadata.rb

"cookbook" == "chef repo"

* metadata for the cookbook
* Contains hints to chef server so that cookbooks are deployed correctly

Chef server tries to minimize what cookbooks it delivers to a node by

1. figure out which _roles_ and _recipes_ ar assigned directly to that system
2. expand the dependencies that those recipes have
3. deliver that whole set to the node

There are some cases where the chef server does not realise that a recipe has a
certain dependency so the `depends` lines in the metadata help fix this.
    QUESTION: when?

chef server compiles `metadata.rb` into a JSON file - you should not edit this
JSON file directly.

## Run list

All the config settings needed for a node to be put in the desired state

* An _ordered_ list of roles and recipes
    * run in exact order
    * duplicates are run whenever they appear
* stored in the "node object" on the chef server
* maintained and uploaded to chef server using knife

A recipe
    * a "configuration element" within an "organisation"
    * must be stored in a cookbook
    * can include other recipes and be included in others
    * must be added to a "run list" before it can be used by chef-client

A role

* 0 or more attributes and a run list

Each node can have 0 or more roles assigned to it.

You "run" a role "against" a node.
    The configuration details of that node
        TODO: how to find them?
    are compared against the attributes of the role
    then the contents of the roles runlist are applied to the nodes configuration details

When chef-client runs it merges its own attributes and run lists with those contained within each assigned role


# Chef server

### Parts

* nginx as front-end load balancer
* RoR web UI app
* Bookshelf of cookbooks
    * backed by filesystem
    * each cookbook is individual repo
* erlang API server backed by
    1. postgres
    2. rabbitMQ sending stuff to an Apache solr instance

### Environments

* Every org begins with a `_default` environment - cannot be modified/deleted in any way.

Environments basically provide another level to specify attributes at

1. default
2. override

Attributes from an environmetn will be merged with other attributes on the node
to figure out the final set of attributes.

There are 5 ways to create environments

1. subdir of environments/ in chef-repo
2. create a JSON in the chef-repo and put it up
3. use knife to create them directly on server
4. use web ui (chef management console) to create them on server
5. use chef server REST API

# Local chef configuration

## ~/.chef

* chef_shell.rb
    * config file for the chef shell
* knife.rb
    * config file for knife

Also contains my private key for my account on the chef server (not the same as having an account on the box)
Also the private key for the "validator"
    TODO: waht is this?


## /etc/chef/client.rb

???

## /etc/chef/solo.rb

???



# Automatically creating users on the system

The 'users' cookbook pulls the users it should create from a databag called
'users' (default name) on the chef server.

* more info at https://github.com/sethvargo-cookbooks/users

To create a new user in the databag do

1. ???

current policy diffs in /etc/sudoers
```
## Polytech only
vagrant ALL=(ALL) NOPASSWD:ALL
rabidadmin ALL=(ALL) NOPASSWD:ALL

# Members of the group 'admin' may gain root privileges
%admin ALL=(ALL) NOPASSWD:ALL

## all other servers
# Members of the group 'admin' may gain root privileges
%admin ALL=(ALL) ALL
```


