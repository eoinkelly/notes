# Reserved IP Ranges

https://tailscale.com/kb/1134/ipv6-faq/

## RFC 1918

http://www.faqs.org/rfcs/rfc1918.html

The Internet Assigned Numbers Authority (IANA) has reserved the following three
blocks of the IP address space for private internets:

    10.0.0.0        -   10.255.255.255  (10/8 prefix)       (the 24-bit block)
    172.16.0.0      -   172.31.255.255  (172.16/12 prefix)  (the 20-bit block)
    192.168.0.0     -   192.168.255.255 (192.168/16 prefix) (the 16-bit block)

We will refer to the first block as "24-bit block", the second as "20-bit
block", and to the third as "16-bit" block.

Note that (in pre-CIDR notation):

- the first block is nothing but a single class A network number
- the second block is a set of 16 contiguous class B network numbers
    - 16 -> 31 inclusive is 16 class B (16 bit, 65535 host) networks
- the third block is a set of 256 contiguous class C network numbers.

Aside: CIDR = Classless Inter Domain Routing. The "Classless" bit refers to the
fact that the older way of refering to networks grouped them into classes but
this way does not.

To conform to RFC 1918, routers are expected to filter out packets with
addresses in these ranges.

# Large Scale NAT (LSN) aka Carrier-Grade-NAT (CG-NAT)

In situations where you are behind LSN, the external IP of your gateway is also
a reserved address. Using RFC-1918 addresses caused problems (because both sides
of the router could have overlapping IPs so RFC
https://datatracker.ietf.org/doc/html/rfc6598 allocates an additional space for
LSN:

    100.64.0.0/10
    i.e. IP addresses from 100.64.0.0 to 100.127.255.255

Tailscale also uses this range for their tunneled network

Downsides

- -- prevents port forwarding
- -- it's stateful so has scalability problems
- -- you cannot host websites behind it because you can't have a stable public
  IP address
