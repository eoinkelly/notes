# Learning Chef

## Tips

* Always use absolute paths to files and dirs when representing them as resources
* chef is just ruby so you can interpolate strings and things like `ENV` as normal
* chef runs shell commands in powershell not cmd on windows

## Overview

You describe _what_ you want your configuration to look like in chef, not _how_

## Multiphase execution model

* chef has a _multiphase_ execution model
    * the multiphase model allows you to use loops etc. to build resources

1. Phase 1: ruby files are evaluated, resource objects are built
2. Phase 2: resoure objects are run

chef resources conceptually work like this:

```ruby
resource 'name' do
  param1 'value1'
  param2 value2
end

# desugars to

# phase 1
resource = Resource.new('name')
resource.param1 = 'value1'
resource.param2 = value2

# phase 2
resource.run!
```

## Resources

https://docs.chef.io/resource.html

* A resource is a statement of policy configuration
    * it defines the desired state of whatever thing it represents
    * chef will use _providers_ to bring the resource into the desired state if it is not already in it.
* resources are grouped into recipes
* All resources have an `action` property
    * the action property has a sane default value so sometimes you can omit it
* To delete a resource you specify a custom action - chef has no "undo" but you
  can specify a desired state which represents the resource _before_ your chef
  code did anything


Resource types

The first four (package, template, service, cookbook_file are the main ones to care about)
1. package
    * manage packages using the system's package manager
1. template
1. service
    * manage an OS service
1. cookbook_file
1. file
    * manage an existing file
    * `:create` is default action
1. bash
    * takes a (possibly multiline) string and executes it as a shell script
1. chef_gem
    * install a gem inside chef so it is availble to your chef code e.g. `chef_gem "httparty"`
1. deploy_revision
    * manage deployment of code from a repo
1. cron
1. directory
1. execute
    * execute an arbitrary oneline command as if it were entered on the command line
1. gem_package
    * install a gem for use outside chef e.g. bundle-audit `gem_package "bundle-audit"`
1. group
    * manage a group of users
1. link
    * manage symlinks and hard links
1. mount
1. remote_file
    * transfer a file from a remote location e.g. curl, wget (todo: unsure if ftp/sftp/s3 supported)
1. template
    * manage an ERB template
1. user

## Cookbooks

* cookbook names can have `-` or `_` but `-` is discouraged because it can cause problems with custom resources
* the name of a cookbook as it appears in runlists is taken from `metadata.rb` NOT the directory name of the cookbook
    * for sanity, keep those the same
Cookbook file structure

```
.
├── Berksfile
├── Berksfile.lock
├── LICENSE
├── README.md
├── chefignore
├── files
│   └── default
│       └── motd
├── metadata.rb
├── recipes
│   ├── default.rb
├── spec
│   ├── spec_helper.rb
│   └── unit
│       └── recipes
│           └── default_spec.rb
└── test
    └── smoke
        └── default
            └── default_test.rb

11 directories, 11 files
```

The dirs explained:

* attributes
    * when there is more than one attribute file, they are evaluated in alphabetical order
    * by **convention** (but not required) if you have just one attribute file you call it `default.rb`
* recipes
    * chef "evaluates" only recipes on its run-list
        * does that mean it doesn't even eval the ruby?
    * by **convention** (but not required) if you have just one recipe file you call it `default.rb`
* files
    * is the place to store files (images, text etc.) that you want to copy to the node from the cookbook via the `cookbook_file` resource
    * the directory structure under `files/` decides which nodes the files should be distributed to
        * you can filter which nodes get which files via subdirs of `files/` e.g.
            * host node name
                * `files/foo.rabidapp.com/somefile`  will only be copied to the filesytem on nodes which match the hostname
            * platform
            * platform version
            * part of the platform version
            * default
                * files in here go to all nodes
        * files in the `files/default/` dir go to **all** nodes
* templates
    * similar to the `files` dir except instead of just copying the file to the node the templates are run through ERB

## Node (the machine)

* a machine managed by chef is a node
    * can be a real machine, VM, container, cloud instance, NAS, network switch, router etc.
        * anything which can install & run chef-client is a node

## Node (the attribute)

* an attribute is a variable maintained by chef
* node is a `Mash` not just a `Hash` so you can use dot notation to get at keys
    * Mash seems to be a magical hashie alike hash
        ```ruby
        # all these are equivalient
        node[:virtualization][:system]
        node['virtualization']['system']
        node.virtualization.system
        ```
    * Mash docs: http://www.rubydoc.info/github/opscode/chef/Mash

### Handling errors

* chef has both notification handlers and exception handlers
* these handlers can do things
    * send emails
    * send alerts to pagerduty
    * log things to console
* they run at the very end of the chef-client run


The runlist

* a ruby array of strings telling chef which recipes to run and in what order
* each string has format `"cookbook_name:recipe_name"`
    * if the recipe name is `default` it can be omitted so `["cookbook1_name", "cookbook2_name"]`
      would run the default recipe in each of those cookbooks.
* sometimes (depends on how you pass the strings to chef) you might have to make each string `"recipe[cookbook_name:recipe_name]"` (yuck if they are actually eval'ing that string as ruby)

Including other recipes

```ruby
include_recipe "cookbook_name::recipe_name"

# if you are referencing a recipe in the same cookbook:
include_recipe "recipe_name"
```

* `include_recipe`
    * can be anywhere in a recipe
    * expands to that recipe's code in exactly that spot
* they recommend keeping recipe files to 1-2 screens of code - break them up with `include_recipe` if they get longer
* some people use a `_` prefix on recipes which are not intended for use by external cookbooks i.e. "private"

## Attributes

Precedence

1. Automatic (highest)
    1. Defined by ohai (highest)
1. Override
1. Default (lowest)
    1. Defined in a recipe
    1. Defined in an attribute file (lowest)

* When setting an attribute in a cookbook you should use `default` precedence most of the time
* when duplicate attribute values are set at the same precedence level then the last one set wins
* when defining an attribute in a recipe you have to use `node.`
* values set in an attribute file are intended to be "defaults" which can be overridden in recipes

Find out where  an attribute is set using `node.debug_value("name_of_attr")

```ruby
node.debug_value("ipaddress")
# [["default", :not_present],
#  ["env_default", :not_present],
#  ["role_default", :not_present],
#  ["force_default", :not_present],
#  ["normal", :not_present],
#  ["override", :not_present],
#  ["role_override", :not_present],
#  ["env_override", :not_present],
#  ["force_override", :not_present],
#  ["automatic", "192.168.1.69"]]

node["kernel"]["name"] # => "Darwin"

# this is how you dig multiple layers into node with #debug_value
node.debug_value("kernel", "name")
# => [["default", :not_present],
#  ["env_default", :not_present],
#  ["role_default", :not_present],
#  ["force_default", :not_present],
#  ["normal", :not_present],
#  ["override", :not_present],
#  ["role_override", :not_present],
#  ["env_override", :not_present],
#  ["force_override", :not_present],
#  ["automatic", "Darwin"]]
```

# define an attribute in a recipe:
# Format is:
#
#     node.{precedence}{attribute name} = {attribute value}
#
# (note the required 'node.' prefix)
#
node.default["a"]["b"] = "value"

# define an attribute in an attribute file:
# Format is:
#
#     {precedence}{attribute name} = {attribute value}
# (note the required 'node.' prefix)
#
default["cookbook_name"]["a"]["b"] = "value"
```

By convention when setting an attribute in an attribute file, the top level key in the attribute name should match the cookbook
## Tools

### chef-apply

* a wrapper around chef-solo
* convenient for running chef ruby files directly locally
* pros/cons
    * -- can only run one file at a time

```
$ chef-apply my_chef_recipe.rb
```

## Test kitchen

* uses the "test-kitchen" gem
* Works with vagrant and virtualbox to create a local sandbox
* kitchen uses vagrant to manage environments
* a kitchen "suite" is
    1. a set of recipes to run on the node
    1. a set of attributes to go with those recipes
* A kitchen configuration is created as part of `chef generate cookbook ...`

```
kitchen init --create-gemfile
# edit .kitchen.yml
kitchen list
kitchen create # vagrant up
kitchen login # vagrant ssh
kitchen setup # installs chef, no direct analogy to vagrant

kitchen converge # run chef client
kitchen destroy
```

Things kitchen does when you run it:

1. install `chef-client` on the test VM
1. create required files under /tmp/kitchen
    * validation.pem
    * client.pem
    * client.rb
    * dna.json
1. copy cookbooks to /tmp/kitchen/cookbooks
1. Run chef-client in local mode
    * The full command line used is `chef-client --local-mode --config /tmp/kitchen/client.rb --log_level --chef-zero-port 8889 --json-attributes dna.json`

### chef-client

* typically run daemonized on a node, checking for updates on an interval
* has 3 modes
    1. local-mode
        * simulates chef server in memory (using chef-zero)
        * any data written to the server is saved on disk in the `./nodes/` dir (relative to your cwd when you run chef-client
            * data is saved as a JSON file
    1. client-mode
        * expects a real chef server on a separate machine
    1. solo mode (deprecated, will eventually be removed)
        * limited subset of 'local mode'
        * exists because it was around before local mode became a thing
            * many larger/older chef customers still rely on it
        * does not support "writeback" of the settings to a local json file like local-mode does

```bash
# put some chef code in hello.rb
$ chef-client --local-mode --log-level info hello.rb
```

Chef client has 5 phases:

1. build the node object (using JSON from `ohai`)
2. Synchronise cookbooks from the server
3. Load code
    1. Load `libraries/*.rb` from every cookbook
        * libaries of ruby code (think lib/ in other ruby projects)
            * will usually inherit from a built-in chef class e.g. `class Chef::Resource::MyDbThing < Chef::Resource::Database; end`
    2. Load `attributes/*.rb` from every cookbook
    3. Load `definitions/*.rb` from every cookbook (deprecated since 12.5, use custom resources instead)
        * Definition is similar to a compile-time macros that can be used in multiple recipes
    4. Load `resources/*.rb` from every cookbook
    5. Load `providers/*.rb` from every cookbook
    6. Load `recipes/*.rb` from every cookbook
4. Converge
    * this is the step where chef makes changes to your system to make it align with the policies stated in your resources
5. Report
    * notification and exception handlers are run here

### Ohai

* collects system information and outputs it as JSON
* can be used as a separate tool if you like
* chef-client converts output from ohai into the basis of the `node` object

```
$ ohai | less
```

### chef

```
# create a cookbook in ./thingy
chef generate cookbook thingy

# add a file to the cookbook under ./files/cookbookname/somefile
cd ./thingy
chef generate file somefile
```

* before chef 13 `knife` was used to create cookbooks but now `chef generate` must be used.
* chef calls the process of deploying a cookbook to a node "converging"

### delivery

> The Delivery CLI is the command-line interface for the workflow capabilities
> in Chef Automate. It sits in-between a local git repository and the Chef
> Automate server.

### chef-vault

* a way of distributing encrypted data bag secrets
* requires you to have a chef-server
* uses the `knife vault` command

## chef-client cookbook

Is handy to:

1. makes it easy to configure chef-client as a service or cron job
2. deletes _validation.pem_ after the first chef-client run

Aside: I skipped chapter 9, 10 because its all about chef server

### chef-zero

* standalone in-memory chef server
* is a separate gem, not part of chefdk for some reason

```
# Setup a playground for working with chef-zero
mkdir -p chef-playground/.chef
ssh-keygen -f validation.pem -P ""
ssh-keygen -f devhost.pem -P ""

# create .chef/knife.rb (see content in this dir)

# terminal 1
# start chef-zero on the port that is configured .chef/knife.rb
chef-zero --port 9501

# terminal 2
cd ./code/chef-playground

# knife will look for $HOME/.chef/knife.rb and then work its way up from the
# cwd looking for a knife.rb if it can't find one there
knife upload .
knife environment show dev
knife environment edit dev
```

## data bags

* the only built-in way to share data between nodes
* format
    * JSON array of objects
    * each object in a single databag must have the same schema (objects in different data bags can be different)
* on disk each data bag is a dir and each item a separate JSON file

```
# example
path/to/data_bags/bag_name_1/item_1.json
path/to/data_bags/bag_name_1/item_2.json
path/to/data_bags/bag_name_1/item_3.json
path/to/data_bags/bag_name_2/item_1.json
path/to/data_bags/bag_name_2/item_2.json
path/to/data_bags/bag_name_2/item_3.json
```

Testing data bags locally

???
```

```

Encrypted data bags

* Encrypted databags use a symmetric key (any string of random bytes will do)
* just add the `--secret-file ./path/to/key` option to knife to use encryption with that operation
* obviously you cannot search for data within encrypted data bags

```
# create a key with openssl
$ openssl rand -base64 512 | tr -d '\r\n' > encrypted_data_bag_secret
```

## roles

* let you collect recipes and attributes that make sense for a particular role
* roles are not versioned on the chef server - this creates problems
    * if you change the runlist for a role you could break existing nodes which used the old runlist
    * devs often work aorund this by making a "role cookbook"
        * a cookbook which includes all the recipes required for that role
        * cookbooks are versioned so this works out better

```js
{
  "name": "webserver", // required
  "description": "Web Server", // required
  "json_class": "Chef::Role", // required
  "chef_type": "role", // required

  // lets you create a runlist for this role
  "run_list": [
    "recipe[motd]",
    "recipe[users]",
    "recipe[apache]"
  ],

  "default_attributes": {
  }
}
```
## environments

* you use enviornments in chef to model your deployment environments e.g. development, test, staging, production etc.
* default environment is called `_default` (note the leading underscore)
* are managed the same way as data bags and roles
* each environment is a JSON file which contains a top level object which contains:
    * required keys:
        * name: String
        * description: String
        * `"json_class": "Chef::Environment"`
        * `"chef_type": "environment"`
    * optional keys:
        * override_attributes: Object
        * default_attributes: Object
        * cookbook_versions: Object
* environments can contain attributes under the `"override_attributes"` and `default_attributes` keys
    * environment attributes override those in recipes and attribute files but are below ohai and roles in precedence
    * Attribute precedence (simplified)
        1. defined automatically by ohai (highest priority)
        2. defined in a role
        3. defined in an environment
        4. defined in a recipe
        5. defined in a attribute file (lowest prirority)

```js
// environment example
{
  "chef_type": "environment" // required, always this value
  "json_class": "Chef::Environment", // required, always this value
  "name": "dev", // required
  "description": "", // required

  "cookbook_versions": { // pin certain cookbook versions in this env
    "couchdb": "= 11.0.0"
  },


  // these attributes are below "normal" level precedence
  // https://docs.chef.io/environments.html
  "default_attributes": {
  },

  // these attributes are above "normal" level precedence
  // https://docs.chef.io/environments.html
  "override_attributes": {
  },
}
```

Managing environments

```
./environments/envname.json

# load an environment from a JSON file
$ knife environment from file envname.json

$ knife environment show envname
```

## Testing

Chef provides ? testing tools

1. FoodCritic (fast feedback)
    * runs in editor, lints code style
1. ChefSpec (medium feedback)
    * run before you even deploy to the test node
    * uses rspec directly - is a set of matchers & helpers for rspec
    * it mocks out a chef run and lets you express expecations about what should have happened
    * does not actually perform any provisioning actions!
    * designed to be fast-feedback unit test to see if you ruby code/logic is consistent
    * run it by invoking rspec in your cookbook root
    * files live under `spec/` withing your cookbook (same as rspec specs usually do)
    * you probably need to `gem install chefspec` before you can run it
1. Inspec or ServerSpec (slowest feedback)
    * run after you even deploy to the test node

ServerSpec vs Inspec
    * ServerSpec is an OSS project and lets you use rspec to verify the state of a server no matter how that server was setup (chef, puppet, ansible, manually)
    * Inspec is created by chef itself and does similar things but for chef only
    * ServerSpec specs live under `test/integration` in your cookbook
    * Inspec specs live under ????


Running tests

```
kitchen verify # runs the tests

# kitchen test is the one to run in CI
kitchen test # does destroy -> create -> converge -> setup -> verify -> destroy
```

### foodcritic

* a chef specific rubocop

```
$ cd path/to/cookbook
$ foodcritic .
```

UP TO END start of roles in chap 15

Unlike when communicating with a real Chef server, which requires the client key to be generated on the Chef server itself, chef-zero accepts any key in a valid format

