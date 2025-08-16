# Elastic IP Address

- Use when:
    - you need to provide a static IPv4 IP for some reason
        - maybe the thing you exchange traffic with cannot be configured to use
          a hostname
- AWS Console has an interface for managing EIPs under both EC2 and VPC (they
  are the same EIPs)
- IPv4 only
- is associated with one of
    1. a network interface
    1. an EC2 instance
- if you add an elastic IP to an EC2 instance
    - the any existing public IP on the instance is released back into the pool
      for AWS to use.
    - the public DNS hostname changes to match the EIP
- If an EC2 instance does not have a public IP address you can associate it with
  an elastic IP to enable communication with the Internet
- The public DNS hostname of an instance is resolved to
    - the EIP if coming from outside the network the instance is on
    - the private IP address if coming from the same network as the instance
- You can associate and disassociate and IP with a running instance at any time
- an EIP can remain active in your account even if it is not associated with
  anything
- Charges
    - there is a small charge if you have an EIP which is not associated (to
      encourage you not to do that)
    - there is no charge for the first EIP associated with an instance but all
      others are charged for.
- If you want to send email from an instance they recommend using an EIP and
  telling AWS about it so they can work with ISPs to help prevent it from being
  marked as spam.
- By default, all AWS accounts are limited to 5 Elastic IP addresses per region,
  because public (IPv4) Internet addresses are a scarce public resource.
    - They recommend using hostnames for all inter-node communication and only
      use an EIP when you absolutely need to.
