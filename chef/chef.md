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


## Add a new staff member to the users data bag

The reference docs for this are https://github.com/sethvargo-cookbooks/users

This will setup an account for them on each production server once
`chef-client` is run on that server again.

In this example _janedoe_ is the name of the new user you are setting up.

#### 1. Get stuff from the new staff member

Get them to send you:

1. A public SSH key (will allow them to SSH without a password)
2. A hashed version of their chosen password. They can get this by running:
    ```
    $  openssl passwd -1 "a password of their choosing"
    ```
    Note the space at the start of the command - this keeps it out of your shell history

#### 2. Add them to users data bag

Create a new item in the `users` data bag by running (on your own machine):

```
$ knife data bag create users janedoe
```

Use this template as a starting point and replace the appropriate bits with their info

```json
{
    "id": "janedoe",
    "groups": [
        "admin"
    ],
    "uid": 1234,
    "full_name": "Jane Doe",
    "shell": "/bin/zsh",
    "ssh_keys": [
        "THEIR SSH KEY HERE"
    ],
    "manage_files": true,
    "password": "THEIR PASSWORD HASH HERE"
}
```

#### 3. Run chef client on each production server

`chef-client` will pull down the `users` data bag and make the users on the system match it.

```
you@local $ ssh you@someproductionserver.co.nz
you@someproductionserver $ sudo chef-client
```


## Aside

```
# see which groups you are in
$ groups

# see info about all users
$ sudo cat /etc/passwd
```

# To delete a user

```
sudo userdel newuser
sudo rm -rf /home/newuser
```

# Setup a new node to use chef using the server

https://wiki.opscode.com/display/chef/Configuring+Chef+Client
Starting with a vanilla Ubuntu 14.04 install

first create a node on the server with a runlist


then bootstrap the node
```
# to bootstrap my vagrant vm, run on my workstation

# remove old host from ~/.ssh/known_hosts
# remove the client and node from the chef server

trying
no_lazy_load true
in client.rb to stop 403 errors

knife bootstrap 33.33.33.10 -x vagrant -N vagrant_kete_test --sudo -r "chef-cookbook::recipe-name"

# this starts chef-client running as a daemon on the target box
ruby /usr/bin/chef-client -d -P /var/run/chef/client.pid -c /etc/chef/client.rb -i 1800 -s 20 -L /var/log/chef/client.log
# it defaults to running once every hour (with a splay of 20 seconds)


me@workstation $ berks vendor cookbooks
```

# test a cookbook with vagrant

First r



# Upload cookbook to chef server

```
cd path/to/your/chef/repo
berks install
berks upload
```

# Users vs clients

"clients" are instances of "chef client" running on a node
"users" are humans who have access to the chef server

As a human using the WebUI of the chef server, the "chef-webui" client is actually what is talking to the chef server.

client and user is to distinguish between human and program

users can use the chefAPI and (through the chef-webui "client") the webUI
clients can only use chefAPI

users can have the admin flag
clients can have admin or validator flags


Examples of client: chef-validator

* an instances of a "chef client"
* The validator is a special client that has one purpose only: to allow nodes to register themselves as clients on the Chef server.
* It's used from inside the node on the first Chef run.
* Once the node is registered, it's good practice to delete the validator key from the node.


# Comms between chef client and server

Comms between client and server are always encrypted.

The _first_ time chef-client is run it:

1. is not registered as a "client" on the server
2. does not have a private key to use for exchanging encrypted messages between client & server

so it cannot connect to the server as the representative of the node it is on.

To get around this 'chef-client' on the node connects as the 'chef-validator'
client the first time it talks to the server. In this first conversation it:

1. registers itself as a new client on the server
2. registers its node in the node list (using the same name as the client!)
3. gets a keypair for future communication with the server - on the node it is stored in `/etc/chef/client.pem`

Once this first conversation is complete, the 'chef-validator' can be deleted from the node
I think this just deletes `/etc/chef/validation.pem` from the client
You can use the `delete_validation` recipe from the `chef-client` cookbook to do this.

    QUESTION: how is "chef-client" a cookbook as well as a binary?
                does the binary run the cookbook?

WHen you use `knife` binary to access the server - you access it as a "user" of the server, not a client.

Implications

* chef-validator is not a separate process
* it is confusing that `chef-client` the binary is easily confused with "chef clients" (the accounts that robots have on the server)
* you can think of `/etc/chef/validation.pem` as a sort of "bootstrapping private key"
* scripts/programs/robots connect to the server as _clients_, humans connect as _users_ (via knife)


# ohai

* Does "operating environment" detection e.g.
    * how much memory
    * what software installed
of the node that is run on
* has plugins that let it understand more things e.g.
    * nginx plugin lets it understand the nginx config

I think it is run by `chef-client` binary to discover stuff about the node that is then pushed into the "node" structure on the chef server.

# chef solo

A _limited functionality_ version of `chef-client` that does not use a chef server

It requires that the cookbook and all its dependencies be on the same physical disk as the node

It does not support things taht would require the chef server to work e.g.
* node data storage
* search indexes
* centralized distribution of cookbooks

It can be run as a daemon
It is configured using the `chef-solo` executable
    => it is NOT the `chef-solo` executable.

It can pull cookbooks from a filesystem dir or a URL that has a .tar.gz

It is configured by the `solo.rb` file
    where???

* It looks for _data bags_ as JSON files in `var/chef-solo/data_bags`
    * can be configured with `data_bag_path` in `solo.rb`
* It looks for _roles_ as JSON or ruby DSL files in `var/chef/roles`
    * can be configured with `roles_path` in `solo.rb`
* It looks for _environments_ as JSON or ruby DSL files in `var/chef/environments`
    * can be configured with `environments_path` in `solo.rb`

# Debugging chef recipes

1. start-up a fresh chef-server in vagrant on 33.33.33.50
1. start-up a fresh test-node in vagrant on 33.33.33.10



Run chef-client

```
$ sudo chef-client --log-level debug
```

# Rbenv cookbook

The rbenv cookbook installs rbenv as a separate `rbenv` user who is a member of the `rbenv` group. Any other users who are also in that group will have more access to the files owned by the `rbenv` user.


# LWRP

Lightweight Resource Provider

* Defined in a cookbook
* Extends chef-client with custom actions
* Has 2 parts
    1. lightweight "resource"
        * defines set of "actions" and "attributes"
        * controls "what" should happen
    2. lightweight provider
        * tells chef-client how to handle each "action" and what to do if certain conditions are met
        * controls "how" it happens

* becomes a ruby class within an organisation
* chef-client has a bunch of existing built-in resources
* "platform resourses" and "platform providers" have a similar relationship to each other

I think the things you invoke in cookbooks are "resources"
"resources" define the "desired state" of the the item and the "provider" knows "how" to get it to that state

template resource
    * uses Chef::Provider::File::Template provider

```ruby
template "/path/to/file" do
    attribute
    action :create # :create|:create_if_missing|:touch|:delete
end
```
