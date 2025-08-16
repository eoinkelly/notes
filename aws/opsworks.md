# OpsWorks

Has 3 parts

1. Stacks (chef managed nodes without a chef server)
1. Chef automate (managed chef servers)
1. Puppet (managed puppet master servers)

## Stacks

Terminology

- Stack
    - a collection of EC2 instances AND related resources (e.g. RDS)
    - a web application typically requires application servers, database
      servers, load balancers, and other resources. This group of instances is
      typically called a stack
    - can have other resrouces "registered" with it:
        1. volumes
        1. Elastic IPs
        1. RDS databases Question: what does "registering" do?
    - You can add easily add tags to the whole stack or a whole layer
- Layer
    - a collection of EC2 instances
    - a stack has 1+ layers
    - A layer represents a set of EC2 instances that serve a particular purpose,
      such as serving applications or hosting a database server.

Layers depend on Chef recipes to handle tasks such as installing packages on
instances, deploying apps, and running scripts.

- Supports both Chef 12 and Chef 11 agents (both of which are pretty old now)

- AWS OpsWorks Stacks does not require or create Chef servers;
- AWS OpsWorks Stacks performs some of the work of a Chef server for you.

Users

- Stacks maintains its own set of users but links them to IAM users
- You can "import" IAM users to be "Opsworks users"
- you cannot create an opsworks user in OpsWorks - you have to import them from
  IAM or another OpsWorks region
- if you add a public key to the imported user they are created as a user on the
  managed instances
- users have assigned "permissions" which lets you layer permissions on whatever
  IAM gives them access to - it lets you give that user certain permissions on
  stuff in a particular stack even if their IAM prevents it

Monitoring

- You can see monitoring aggregated across all instances in a layer

Time and Load based instances

> If you have created apps for the instances' layer or created custom cookbooks,
> AWS OpsWorks Stacks automatically deploys the latest version to time-based and
> load-based instances when they are first started. However, AWS OpsWorks Stacks
> does not necessarily deploy the latest cookbooks to restarted offline
> instances. For more information, see Editing Apps and Updating Custom
> Cookbooks.
