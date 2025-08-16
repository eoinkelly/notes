# Border Gateway Protocol

Sources

- https://jvns.ca/blog/2021/10/05/tools-to-look-at-bgp-routes/

Overview

- Current version is BGP 4
    - https://tools.ietf.org/html/rfc4271
    - In use on Internet since 1994
    - Standardized since 2006

> BGP neighbors, called peers, are established by manual configuration among
> routers to create a TCP session on port 179. A BGP speaker sends 19-byte
> keep-alive messages every 60 seconds[5] to maintain the connection

> When BGP runs between two peers in the same autonomous system (AS), it is
> referred to as Internal BGP (iBGP or Interior Border Gateway Protocol). When
> it runs between different autonomous systems, it is called External BGP (eBGP
> or Exterior Border Gateway Protocol)

## Autonomous System Number (ASN)

- globally unique number
- allows the org to talk BGP with other ASes
- a group of 1+ IP prefixes run by 1+ network operators that maintain a single
  clearly defined routing policy.
- all ASNs should be considered 4 byte
    - prior to 2007 they were 2-byte but were extended into 4 byte
- ? kinds

1. Multi-homed AS
    - connects to 2+ other ASes
    - has multiple connections for redundancy
2. Stub AS
    - connects to only one other AS but can have private connections not visible
      to rest of Internet
3. Transit AS
    - acts as link between two or more other ASes

https://bgpview.io/

# Get from IP address to ASN

Options

1. https://asn.cymru.com/cgi-bin/whois.cgi
2. https://team-cymru.com/community-services/ip-asn-mapping documents how to use
   whois/dns to hit the same service as above

# Find out more about an ASN

- https://bgp.tools/
    - gets real-time feeds of BGP routes that you can query if you know the ASN
    - https://bgp.tools/kb/api (whois/nc compatible API)

Can I get the full BGP table somehow?
