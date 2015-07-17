# AWS

# Services

* Elastic cloud compute (EC2)
    * virtual servers called "instances"
    * Sub service: Elastic load balancing
        * distributes incoming traffic across EC2 instances
        * provides health monitoring of running instances
        * can create "security groups" to set security options
        * can send traffic to instances in multiple AZs
        * pricing
            * pay by hour and pay by GB that is transfered through the load balancer
* Identity and access management (IAM)
    * ???
* Route53
    * A DNS service
    * Can do "latency based routing"
        * Amazon keeps a database of latency info based on ???
        * When a customer does a DNS lookup for your app to Route53 it will get
          back the IP of whichever set of your EC2 instances has the lowest
          latency for that customer
            * => only works if you have instances in multiple regions
    * Can do "weighted round-robin sets" routing
        * Presumably it sends data to each instance in turn but tweaked by the weighting you apply
* Glacier
    * Low cost storage for files you don't need to access very often
    * is a REST based web service (resources: vaults, archives, jobs, notfications)
    * vaults
        * each vault lives in a single aws region
        * each vault has a unique URL http://<region-specific-endpoint>/<account-id>/<vault-name>
    * There will be a delay of several hours to access files from glacier
    * "archive" is the base unit of storage. "archive" is any file (photo, video, document etc.)
    * archives are stored in "vaults"
    * The management console is quite limited - can only create or delete vaults - everything else has to be done over API (via either custom code or the AWS-cli tool)
    * There are SDKs for Java, .Net
    * S3 can be told to automatically archive old data to Glacier. This can
      save money but you are also charged for the transfer so be careful!
    * To get stuff out of glacier
        1. initiate a job
        2. wait for job to complete
        3. download the job output (the vaults and archives you requested)

# Managing users

There is an AWS "root" account
* you can create other "IAM" accounts that have less permissions
* Don't programmatically access the root account (similar to not SSH as root)
* Recommended to create an IAM user for myself with administrator privileges and use that for day-to-day work
* IAM has the notion of groups which get perms assigned
    * similar to groups on a unix box: more flexible than assigning permissions to a single user
* IAM users
    * Get their privileges assigned by the root user
    * Have a unique login link

# Policies

* Amazon has ~148 "policy" objects that you can "attach" to a user, group, role
* You can create your own policy objects
* A policy joins a group of "actions" with a group of "resources" via an ALLOW or DENY
* A policy is a JSON object
* A policy is a bit like a firewall rule - you can allow/deny access to sets of
  actions within a certain amazon service and optionally restrict to
  particular amazon resources
* A policy object can have many users/groups/roles attached and a
  user/group/role can have many policy objects attached
* A "managed policy" is one what is kept in the IAM policy repository
    * A managed policy can references up to 2 entities e.g. user/group/role.
    * Managed policies are automatically updated by Amazon when new features become available
* An "inline policiy" can be pasted inline into a particular user/group/role
* Policies are written in _IAM Policy Language_ (A dialect of JSON)

# Terminology

## Amazon Resource Name (ARN)

* A globally unique ID for a particular "resource"
* Resources are: users, groups, roles, ???

## Principal

* A principal is an entity in AWS that can perform actions or access resources
* A principal can be any of:
    1. An AWS account (the "root" user)
    1. An IAM user
    1. AN IAM group
    1. An IAM role
* A principal is a thing that a policy can be attached to or can be referenced in a policy

## Instance profile

???

## SAML

SAML 2.0 is an XML-based protocol that uses security tokens containing
assertions to pass information about a "principal" (usually an end user) between
a SAML authority, that is, an identity provider, and a SAML consumer, that is,
a service provider

# Role

* A _set of permissions_ that grant access to perform certain _actions_ and access _resources_
* Roles can come from
    * The AWS account you are currently in
    * Another AWS account
    * Some AWS Services e.g. EC2
    * Any 3rd party identity provider which is compatible with SAML 2.0 or OpenID connect
* The permissions for role `Foo` are held in a policy associated with `Foo`
* Each role has 2 policies associated with it
    1. Trust policy: who is allowed to assume the role
    2. Permissions policy: what can they do when they assume the role
        * specifies what _actions_ and _resources_ a user with the role can access
* Roles can be accessed in 2 ways
    1. Via the AWS web console
    2. Via the API using the AWS CLI
* Assuming a role is different in web-console and API:
    * When a principal takes on a role in the web console it loses its existing
      permissions for the duration of having the role
    * When the AWS CLI changes role via `AssumeRole` it gets a new set of
      credentials that it can use to send requests as that role - it does not
      lose its existing credentials. It "exits" the role by just not sending the
      new credentials it got with a request.
* Use cases for roles
    * Grant access to an IAM user in a different account to some actions or
      resources in your account
    * Temporairly switch roles in my own account when actions or resources
      things that are easily damaged - similar to how we use `sudo`

# Regions and availability zones

* Region = a geographic area
    * Regions are (by design) completely isolated from each other
    * Examples of _regions_:
        * eu-west-1 (Ireland)
        * ap-southeast-2 (Sydney)
    * When you view resources in the web console, you only see those defined in
      the region you have currently selected.
    * A region has multiple AZs
* Availablility zone = the data center in a specific geographic area
    * AZs within a region have low-latency links connecting them
    * AZs are architected to not go down if a different AZ goes down
    * Examples
        * The `ap-southeast-2` region (Sydney) currently has 2 AZ
            * `ap-southeast-2a`
            * `ap-southeast-2b`
* Resources can be
    1. Global
        examples: IAM
    2. Tied to a specific region
        * examples: S3 Bucket
    3. Tied to a specific AZ
        * examples: EC2 Instance
* How-to:
    * To see AZs: log in, choose a region from top-right, choose EC2 service.
      AZs are listed at bottom of page
* Most services support regions
* Some do not e.g. IAM
* The resources in each region are independent
* Resources are not automatically replicated across regions!
* S3
    * If you make an S3 bucket that is not in the default region you need to
      specify region in your API requests via a "location restraint" e.g. for
      Sydney it would be `ap-southeast-2`
    * The S3 console is global (you can see all regions at once)
    * Each bucket is created in a specific region
* EC2
    * When you create an instance you can choose a region _and_ an availability zone
    * Spreading your instances across both is a good idea for fault tolerance
    * You may not be able to choose a particular AZ if it is near capacity or Amazon are winding it down.
    * There does not seem to be a way to see all your EC2 instances in all regions at the same time
* RDS
    * a web service that lets you manage a datbase server
    * relational databases in the cloud
    * can create instances of
        * MySQL
        * Oracle
        * PostgreSQL
        * Microsoft SQL Server
    * you do not have full shell access - some advanced admin is blocked off
    * it does automated backups
    * with their "on demand" instances you pay by the hour for whatever compute your DB uses - the tier you pick sets max/min storage sizes
    * You can also run postgres on an EC2 instance which means you have to do more admin yourself but is probably bit cheaper (not verified)
    * they have their own fork of Postgres
    * > It's more oriented towards the DBA, Devops staff or Sysadmin than Heroku's service
    * Have seen reports that you cannot replicate into/out of RDS - still need to use Postgres on EC2 for that

## Tags

* A tag is a label you apply to a resource
* tag is a case sensitive `name: value` pair
    * keys cannot bwgin with `aws:` as it is reserved
* uses:
    * categorize resources e.g. based on owner, purpose
    * ideas for tag labels:
        * Owner (who is responsibile for various resources)
        * Purpose (what business goal is this achieving)
        * Client (allows you to split up billing them based on the resources they use
        * Organisation section
        * Environment (production, pre-prod, staging etc.)
        * ...
* tags are used to categorize your bill so you can see how much various categories are costing you

* Tags also allow you to put resources into "resource groups"

# Codedeploy

* parts:
    * an agent that runs as daemon on the AWS instance OS
    * ???
* appspec.yml
    * a collection of file copy commands and shell-script run commands grouped into hook events
    * scripts are run in the order they appear
* AWS CodeDeploy calls a set of instances a _deployment group_.
* A deployment group contains
    * individually-tagged instances,
    * Amazon EC2 instances in Auto Scaling groups,
    * or both.
* How-to
    1. put appspec.yml in the root of your project
    1. create a zip of the project and put it on S3
    1. trigger the deploy (various methods e.g.
        * aws cli tool
        * from CI server
        * github hook

