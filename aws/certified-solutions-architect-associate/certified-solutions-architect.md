# Certified Solutions Architect - Associate

The exam

- 130 minutes
- 60 questions
- results between 100-1000, passing score 720 (approx 70%)
- The CSA-A exam is a superset of the _Cloud Practitioner_ exam
- 2 kinds of question
    1. 4 options, one is correct
    2. 5 options, two are correct
- The professional exam is all about time management

- _US-East North Virginia_ is the oldest region (not required for exam)
- As of 2019-03-06
    - 20 regions (4 planned for 2019)
    - 61 AZs (21 planned in 2019)
    - 166 "Points of Presence" (155 Edge Locations and 11 Regional Edge Caches)
- the numbers above imply there is usually 3 AZs in a region

#### Points of presense (edge locations & regional edge caches)

- Edge locations & regional edge caches are collectively known as "points of
  presense"
- https://aws.amazon.com/cloudfront/features/
- There is both an edge location and a regional edge cache in Sydney
- Services which use these points of presense:
    - Cloudfront
    - Route53
    - AWS WAF
    - AWS Shield

#### Regional edge cache

- an extra layer of caching sitting between origin server and edge location
- nothing to configure, happens automatically
- have larger caches than the edge location so can keep objects after the edge
  location has evicted them
- reduces traffic back to your origin server

#### Local regions

- think of them as "region lite"
- a region with just one AZ i.e. one datacenter
- examples: Osaka
- you have to go through a sales rep to use it
- "Local regions" are intended to be used with a nearby normal region e.g. Osaka
  is intended to be used with Tokyo

AWS support plans

https://aws.amazon.com/premiumsupport/plans/

Sometimes these come up on exam

1. Basic Plan
    - Free
1. Developer Plan
    - Starts at $29/month (inceases based on usage)
1. Business Plan
    - Starts at $100/month (inceases based on usage)
1. Enterprise Plan
    - Starts at $15k/month (inceases based on usage)
    - contact account manager for pricing

- Access to all AWS services except the management of groups and users within
  IAM. How does IAM integrate with ActiveDirectory (if at all)

REWATCH UP TO END CHAP 3

Power user access level:

## Cloudfront

### Edge locations

Services offered at edge locations:

- Amazon CloudFront
- Amazon Route 53
- AWS Shield
- AWS WAF services

### Regional Edge Caches

- A feature of Cloudfront
- Sit between your server and the edge location
- Have larger caches than the edge location so may keep your content for longer
- There is an edge location in Sydney
- Are turned on by default with no additional charges

> As the popularity of your objects reduce, individual edge locations may evict
> those objects to make room for more popular content. Regional Edge Caches have
> larger cache-width than any individual edge location, so your objects remain
> in cache longer at these locations.

Elastic Map Reduce (EMR)

- Uses hadoop

Cloud Search

- Fully managed service vs elastic search?

Kinesis

- storing and analyising real-time data at scale

Data pipeline

- "allows you to move data from one place to another" e.g. S3 to DynamoDB,
  DynamoDB to S3

Quick Sight

- business analytics tool
- lets you build dashboards for your data

AWS Inspector

- agent you install on your VM, does reporting

Certificate Manager

- Free SSL

AWS Artifacts

- Lets you download documents which assert your compliance to various standards
  e.g. HPAA, PCI

Cloud trail

Step functions

SWF

- a way of coordinating automated tasks and human led tasks
- is important in the exam

CodeCommit = AWS Github CodeBuild = "compiles your code" CodeDeploy = deploys
code CodePipeline = lets you manage pushing code through your various
environments

Workspaces = AWS Citrix

## Route 53

Sources

- https://www.youtube.com/watch?v=f9y-T7mQVxs AWS re:Invent 2014 | (SDD408)
  Amazon Route 53 Deep Dive: Delivering Resiliency, Minimizing Latency

You can used a private Route53 hosted zone across **multiple** VPCs so if you
have a VPC in different regions for global traffic, you can use the same private
dns everyhwere

## VPC

Traffic goes through the route table **before** the NACL

```
[Internet] <--> [Internet Gateway] <--> [Routeer] <--> [Route Table] <--> [NACL] <--> [Subnet] <--> [Security Group] <--> [EC2 Instance]
```

- You can use route tables to prevent certain subnets from communicating with
  each other
- You can attach **only one** internet Gateway to a VPC
    - Internet Gateways are highly available across AZs

- Peering
    - You can peer VPCs within the same account or across accounts
    - Always in a star configuration - no transatitive peering

QUESTION Should I enable IPv6 on VPCs now?

AWS randomize the mapping between AZ name and the actual data center it uses
between accounts => the `ap-southeast-2a` AZ can point at different data centers
in different accounts

- AWS VPCs do not support network broadcast on the X.Y.255.255 address
    - They reserve that address so you cannot use it to clarify this

Reserved addresses in a 10.0.0.0/16 VPC

```
10.0.0.0 Network Address
10.0.0.1 VPC Router
10.0.0.2 DNS Server
10.0.0.3 Reserved for future use
10.0.0.255 Network broadcast
```

- Security groups do not span VPCs
- You can use a security group name instead of a CIDR range as the source of
  traffic when creating a security group rule
    - Allows you to say things like "instances in secgrpA should be allowd to
      connect to the postgres port on any instance in this secgrp"
    - "instances in my appServer secgrp should be allowed to connect to mysql
      port on this server"
        - this gives you much better granularity than allowing anything in the
          subnet or VPC to connect

Nat Gateways

Hint: NAT Gateways take a few minutes to provision so don't change your route
tables immediately when you create one and expect it to work

A NAT gateway exists in one AZ only so if you want to be resilient against that
AZ going down you should create a NAT gateway in multipel AZs and put them all
in your route tables

NACL

AWS Network ACL Rules (both inbound and outbound) are defined in terms of the
DESTINATION port

## Flow Logs

http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/flow-logs.html

- flow logs capture **IP traffic** (i.e. at the IP layer)
- are not real-time
- Can be configured at 3 levels
    1. VPC level - capture all ENI traffic in the VPC
    1. Subnet level - capture all ENI traffic in a subnet
    1. ENI level - capture all traffic for a single elastic network interface
       (ENI)

Flow logs do not capture **all** traffic - the following traffic is not captured

1. Traffic between the instance and the Amazon DNS server (if you use your own
   DNS servers then traffic is logged)
1. traffic to/from 169.254.169.254 for instance metadata
1. DHCP traffic
1. Traffic to the reserved IP address for the VPC router
1. Traffic generated by a windows instance for "Amazon Windows License
   activation"

Example of a flow log from a single ENI (eni-262d5f0d)

```
version account-id   interface-id srcaddr         dstaddr       srcport dstport protocol packets bytes start      end        action log-status
2       365561110285 eni-262d5f0d 172.31.14.46    91.189.94.10  33050   443     6        15      1906  1513482275 1513482279 ACCEPT OK
2       365561110285 eni-262d5f0d 91.189.94.10    172.31.14.46  443     33050   6        13      6151  1513482275 1513482279 ACCEPT OK
2       365561110285 eni-262d5f0d 91.189.89.198   172.31.14.46  123     51307   17       1       76    1513482301 1513482339 ACCEPT OK
2       365561110285 eni-262d5f0d 172.31.14.46    91.189.89.198 39401   123     17       1       76    1513482301 1513482339 ACCEPT OK
2       365561110285 eni-262d5f0d 91.189.89.198   172.31.14.46  123     39401   17       1       76    1513482301 1513482339 ACCEPT OK
2       365561110285 eni-262d5f0d 172.31.14.46    91.189.89.198 51307   123     17       1       76    1513482301 1513482339 ACCEPT OK
2       365561110285 eni-262d5f0d 74.125.206.197  172.31.14.46  80      63511   6        3       156   1513482341 1513482399 REJECT OK
2       365561110285 eni-262d5f0d 77.72.82.158    172.31.14.46  50842   5446    6        1       40    1513482341 1513482399 REJECT OK
2       365561110285 eni-262d5f0d 91.189.89.198   172.31.14.46  123     48369   17       1       76    1513482341 1513482399 ACCEPT OK
2       365561110285 eni-262d5f0d 172.31.14.46    91.189.89.198 48369   123     17       1       76    1513482341 1513482399 ACCEPT OK
2       365561110285 eni-262d5f0d 219.88.160.81   172.31.14.46  64838   22      6        9       576   1513482341 1513482399 REJECT OK
2       365561110285 eni-262d5f0d 219.132.29.103  172.31.14.46  59251   1433    6        1       40    1513482341 1513482399 REJECT OK
2       365561110285 eni-262d5f0d 181.139.160.223 172.31.14.46  12850   2323    6        1       40    1513482438 1513482459 REJECT OK
2       365561110285 eni-262d5f0d 77.72.82.158    172.31.14.46  51421   3108    6        1       40    1513482438 1513482459 REJECT OK
```

Each record captures the network flow for a specific 5-tuple, for a specific
capture window

- A 5-tuple is a set of 5 different values that specify the source, destination,
  and protocol for an Internet protocol (IP) flow.
    - `(srcaddr, dstaddr, srcport, dstport protocol)`
- The capture window is a duration of time during which the flow logs service
  aggregates data before publishing flow log records.
    - The capture window is approximately 10 minutes, but can take up to 15
      minutes.

Exam tips

- You can only enable flow-logs for peered VPCs if the peered VPC is also in
  your account
- You cannot add tags to a flow log
- Flow logs are immutable - you cannot change its configuration after you have
  created one

VPC Endpoints

- Allows your EC2 instances to access other AWS services (e.g. S3) without
  needing a NAT gateway
    - If you use a NAT gateway then that traffic goes over public Internet, with
      an endpoint it is garuanteed to stay within AWS network
- Two kinds
    1. Endpoints - just an ENI which can be used to access the service
        - Examples: EC2 API, ELB API (also other APIs which it would be handy
          for an instance to access)
    1. Gateways - works a bit like a NAT gateway except you can only access one
       service
        - you add the gateway as a destination in your private subnets route
          tables
        - S3 and DynamoDB use gateways
- Implications
    - If your linux distro has a package repo on S3 you might not need a NAT
      gateway (saves $)
        - AWS Linux does this

## Route53

- SOA = Start of Authority record
    - Name of the server which supplied info about th zone
    - Administrator of the zone
    - Current version of the data file
    - The no. of seconds a secondary name server should wait before checking for
      updates
    - The no. of seconds a secondary name server should wait before retrying a
      failed zone transfer
    - The maximum no. of seconds a secondary name server can use data before it
      must be refreshed or expired
    - The default no. of seconds for the TTL file on resource records
- NS Record

"Content DNS Server" is the name server doing a lookup on behalf of a client
