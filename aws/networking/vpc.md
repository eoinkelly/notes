# VPC

- When should you create a new VPC?
    - Whenver you want a "logically isolated" set of AWS resources.

- Examples (are these good/bad ideas?)
    - Separate VPC per client if you host all clients in one account
    - Separate VPC for each environment if just one client in the account

> We also moved production and staging environments into a single VPC. We also
> share security groups between the environments. We separate the environments
> by using different Subnet Groups and adding Network ACLs that ensure
> production and staging cannot connect to each other.

Private IP addresses on EC2 instances are private to the VPC!

## The default VPC

- You get a VPC in every AWS region by default
- It has IP CIDR `172.31.0.0/16` which puts it right at the top of the 20-bit
  block

## Creating a VPC

- Name - add a name tag to the VPC
    - you don't really give the VPC a name, it just allows you to set a `Name`
      tag at creation time
    - The `Name` tag seems to be special in the AWS Console UI - it is shown in
      tables beside the VPC ID
- CIDR block - the IP range to assign to this VPC e.g. 10.0.0.0/16,
  192.168.0.0/16
    - mask sizes must be between /16 (65536 hosts) and /28 (8 hosts)
    - maximum VPC network size is /16 on AWS
    - You can use _any_ CIDR range as long as it has size between /16 and /28
    - You can have multiple CIDR ranges
        - QUESTION: when would you want this?
    - So you can create a VPC which can contain between 8 and 65536 hosts (A VPC
      must be in a single region)
        - Then you use subnets to further divide those between AZs
    - A VPC has to be one region and a subnet has to be one AZ so VPCs let you
      define your address space for the region and subnets let you divide that
      up between AZs
- Tenancy options
    - Default
        - Your VM instances use whatevery tenancy you choose when launching them
    - Dedicated
        - lets you specify that instances in this VM should always use single
          tenant dedicated hardware no matter what their launch options are
          configured as
        - WARNING: a lot more expensive!
        - QUESTION: why can you set this as an option at the VPC level?
- DNS Resolution (on/off)
    - toggles whether the AWS DNS will be used in this VPC
- DNS Hostnames (on/off)
    > When you launch an instance into a default VPC, we provide the instance
    > with public and private DNS hostnames that correspond to the public IPv4
    > and private IPv4 addresses for the instance.
    >
    > When you launch an instance into a nondefault VPC, we provide the instance
    > with a private DNS hostname and we might provide a public DNS hostname,
    > depending on the DNS attributes you specify for the VPC and if your
    > instance has a public IPv4 address
- DHCP OptionSets (lets you configure what DHCP options are passed to instances
  in your VPC)
    - You cannot edit sets of DHCP options, if you want to make changes you need
      to make a new OptionSet
    - DHCP lets you pass configure a host via TCP/IP
    - A DHCP message contains an `options` field which contains
        - domain name
        - IP addresses of up to 4 domain name servers
        - netbios node type
        - netbios name servers
        - IP addresses of up to 4 ntp servers
        - others
    - You can configure the options which will be passed to your instances by
      DHCP (a default set of options is provided)
    - When you create a new VPC, an existing DHCP OptionSet is associated but
      you can change it later
    - Amazon provides its own DNS server - if you want to use your own you need
      to create a custom DHCP OptionSet

### Side-effects of creating a new VPC:

The following are 3 things are also created

1. A route table marked as `Main`
    - this route table seems to only allow local traffic between instances in
      its CIDR block
2. A network ACL
3. A security group for the VPC

Note creating a VPC does not create any subnets so we can't deploy anything into
it yet

### Questions

- AWS allows you to associate a /56 IPv6 block with the VPC
    - it does not let you choose the range of addresses - why?

## Deleting a VPC

Deleting a VPC will also delete objects associated with the VPC in this region:

1. Subnets
2. Security Groups
3. Network ACLs
4. VPN Attachments
5. Internet Gateways
6. Route Tables
7. Network Interfaces
8. VPC Peering Connections

So deleting a VPC has quite a blast radius => Maybe don't share those things
between VPCs (I'm not sure how much you even can)

QUESTION: if I had a SG shared between VPCs would it get deleted if I deleted
one of them?

## FLow logs

- Can be created at any time
- Lets you log all the IP traffic in a VPC to Cloudwatch
- You give it a "log group" and then a "log stream" will be created under that
  group for every network interface in the VPC

=======================================================================

General stuff (pasted from the CDA exam notes)

- you get one default VPC **in each region** - don't delete them or you have to
  ring AWS to get one back
- Virtual private cloud (VPC) [VERY IMPORTANT IN EXAM]
    - Virtual data center
    - can have multiple VPCs per region
    - can have multiple VPCs in your account
    - can get them to peer with each other
    - a "data center" which exists in your account
    - each VPC has a logcially isolated set of resources
    - A VPC cannot span regions (but you can peer VPCs across regions)

- most important thing in the exam
- need to be able to build a VPC from memory
- by default you get a VPC based on region when you log in

- VPC is a logically isolated section of the AWS cloud
- VPC can span availability zones
- VPC CANNOT span regions
- you can
    - create your own subnets
    - launch instances into a particular subnet
    - configure route tables between subnets
    - create internet gateways and attach them to subnets
    - choose your own IP ranges
    - create subnet network access control lists (ACLs)
    - create "instance security groups"
- can create public facing subnets for web servers
- can create appliaction servers and DB servers in private subnets
- can create a hardware VPN connection between your VPC and your existing
  corporate datacenter
    - allows you to extend your data center with your VPC
    - an amazon "Virtual private gateway" is the thing you setup in AWS to talk
      to your existing datacenter - it interfaces your data center to your VPC
    - this is called "hybrid cloud"

- default VPC
    - user friendly
    - allows you to immediately deploy instances
    - all subnets have an internet gateway attached
    - all subnets are public subnets
    - each EC2 instance has a public AND private IP address by default
    - WARNING: if you delete the default VPC the only way to get it back is to
      contact AWS
        - you can just change region
    - I seem to have a different default VPC configured in each AWS region
- VPC peering
    - can connect VPCs to each other via a direct network route using private IP
      addresses
    - instances behave as if they are on the same private network
    - You can peer VPCs with other AWS accounts
        - TODO: dig into what this means (this is not part of exam)
    - You cannot peer between regions (you can achive something similar by
      setting up a VPN between the regions but Amazon doesn't do it for you
    - You can peer VPCs with other VPCs in the same account
    - peering is always done in a star configuraiton (with 4 points)
        - one central VPC peers with others
        - consider 5 VPCs peered in a star:
            ```
                   C
                   |
            B <--> A <--> E
                   C
                   |
                   D
            ```
            where A is peered with B and also C. You cannot do transititve
            peering so if you want B and C to peer you have to set it up
            separately

    - NB: you cannot do transative peering!!! (this is examp Q)

- By default you can have up to 5 VPCs in each region

Aside: CIDR = classless inter domain routing

- subnets
    - subnets area ALWAYS in a single availability zone (exam Q)
    - subnets can only be associated with one route table at a time
    - when you create a subnet you can choose which AZ in the region you want it
      to be in
        - but even if you don't, AWS will choose one for you
- You can only have ONE internet gateway per VPC (important exam Q)
- When you create an internet gateway it is not attached by default

1. Create the VPC
    - creates a "main" route table for you
    - creates a Network ACL for you
    - creates a security group for you
1. Create some subnets
1. Add an internet gateway to the VPC
1. Create a new route table which allows access out to the Internet
    - Don't add that route to your main route table because you don't want all
      subnets to default to having outside access
1. If subnet is public, change the "auto assign public IP" setting
1. If you forget to assign a public IP address you can do it alter by adding an
   "Elastic IP address" to the instance

How to allow instances on private subnets to pull in updates from the Intenet?

1. option: nat instance (old way)
    - create a new EC2 instance which amazon have configured to do NAT for you
    - put it in the public subnet
    - the private instances have access to it so can use it to bounce traffic
      out to the internet
    - you must disable the "source/destination check" for the instance because
      the NAT needs to route traffic which isn't to/from itself
    - you can get redundancy by putting your nat instance in an autoscaling
      group
    - if you are bottlenecking you can increase instance size
2. option: nat gateway (new way)
    - deploy it into the public subnet
    - you don't have to manage the instance e.g. no security patches etc.
    - you don't have to worry about redundency - nat gateways do it for you
    - they scale u to 10Gbps
    - once you create a nat gateway you still have to change the route table for
      your private instances to route traffic to it

A "route table" is a collection of routing rules is associated with **subnet**
not instances A subnet has exactly one route table route tables have _
destination (the source IP addresses of instances _ target (where the packets
should go)

Tip: put a "bastion host" or "jump box" in your public subnet so you can access
your private instances via SSH

bastion hosts are used to securely admin private instances - they are often
hardened

IMPORTANT = a subnet cannot span across AZs \* security groups, route tables,
network ACLs can span multiple AZs

IMPORTANT = you can only have ONE internet gateway for each VPC \* they may ask
question implying you should add another Ingernet Gateway to VPC for perf or
security

To make a subnet be publically accessible

1. create a new route table and add a route that will allow all traffic from
   attached subnets to a chosen IGW
    - target: the internet gateway of your choice
    - source: 0.0.0.0/0
2. Associate the subnet with the route table
