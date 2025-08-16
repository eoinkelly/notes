# AWS Certified Solutions Architect Professional

ELB

- are _region wide_
    - can be deployed in multiple AZs within a region
- is fully managed
- can be used internally OR externally
    - you can load balance services within the VPC
- can do layer 7 stuff e.g. SSL termination
    - doing SSL termination on the ELB takes work away from the instances
- supports cookie based sticky sessions
    - allows you to give uses a cookie which will always route a user to a
      particular instance in the pool
        - AWS best practice is to use a database for this instead of on the ELB!
- Integrates with autoscaling
    - the ELB can trigger autoscaling based on load
    - autoscaling can automatically register instances in the ELB once they are
      provisioned
- Gives you health checks
- integrates with cloudwatch
    - lets you do advanced metric load balancing
    - you can apply load balancing based on on the metrics in cloudwatch e.g.
      CPU usage
- Integrates with Route53
- Supports the following ports
    - ports
        - 25
        - 80
        - 443
        - 1024-65535
    -
- You cannot assign a stable IP address (e.g. EIP) to an ELB - you can only
  reference ELBs by their DNS name
- Supports domain zone apex ie.. you can ref ELB without a prefix on the domain
- ELB supports IPv4 and IPv6
- supports cloudtrail for audit logging and analysis

Q: how Questiomany ssl certs can an ELB have? it used to be 1 but maybe that's
diff now?

- There can only be **one** SSL certificate per ELB
    - you can use wildcard certs but you cannot assign multiple certs to an ELB

ALBs can use any port you want but they seem to want the protocol to always be
HTTP or HTTPS - TODO: verify this

Can ALBs use a cloudwatch metric for monitoring? I can't see it in the UI

TODO: paly with ELB using queue depth - I htink they added this recently

Q:Questio do vpcs support IPv6 now?
