# NACLs

## Ephemeral ports

IANA recomments 49152 to 65535 for dynamic/private ports

- Many linux kernels 32687 to 61000
- FreeBSD has used IANA range since release 4.6 Win XP 1025 to 5000 Win 7 and
  later use the IANA range (but they allow user to specify a custom range)

Conclusion: the only safe range of ephemeral ports to use seems to be 1025 to
65535

## General NACL stuff

- A network ACL contains a numbered list of rules that we evaluate in order,
  starting with the lowest numbered rule, to determine whether traffic is
  allowed in or out of any subnet associated with the network ACL.
    - As soon as a rule matches traffic, it's applied regardless of any
      higher-numbered rule that may contradict it.

The highest number that you can use for a rule is 32766.

We recommend that you start by creating rules with rule numbers that are
multiples of 100, so that you can insert new rules where you need to later on.

A network ACL has separate inbound and outbound rules, and each rule can either
allow or deny traffic.

Network ACLs are stateless; responses to allowed inbound traffic are subject to
the rules for outbound traffic (and vice versa)

Network Access Control Lists (or "Subnet access control lists")

- security groups and NACLs provide a two layer protection
- act a bit like a network firewall
- apply rules to a whole subnet
- are "stateless" - e.g. if you want to let HTTP in and out you have to create
  two separate rules
- NACL overrides rules from a security group
- NACLs support both ALLOW and DENY rules (unlike security groups which only
  support only ALLOW rules)
- Security groups act like a firewall at the instance level whereas NACLs are an
  additional layer of security that act at the subnet level
- an ACL is
    - a numbered list of rules
    - rules are evaluated in order **starting** with lowest numbered rule and
      the first rule which matches stops evaluation (exam Q)
    - the `*` rule is the last rule evaluated
    - determines whether traffic is allowed in or out of any subnet associated
      with the ACL
    - highest number is 32766
    - suggest start with rules which are multiples of 100 to allow gaps for
      editing
    - each VPC comes with a modifiable default ACL
    - default ACL allows all inbound and outbound traffic
    - custom ACL start as fully closed (permits no traffic) by default
    - each subnet must have only one ACL
        - if you deassociate a custom NACL with a subnet the subnet will
          automatically be put back in the default NACL
    - each NACL can have 0-N subnets
    - each NACL must live in a VPC (they cannot span VPCs)
    - ACLs are stateless!
        - e.g. they can't allow a response in based on some previous request out
    - each subnet MUST be associated with one NACL
        - if you don't explicitly associate an NACL the default one will be used
    - a subnet can only be one NACL at any time
- default NACL allows all traffic inboutnd and outbound
- when you create a custom NACL all traffic inbound/outbound is blocked by
  default
