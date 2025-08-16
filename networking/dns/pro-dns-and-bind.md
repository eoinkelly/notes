# Pro DNS and Bind book

- DNS is how you get from a name to an IP address
- Getting traffic to/from the IP address is handled by a different subsystem
- Original DNS RFCs date from 1987 - 35 years ago!
    - RFC 1034
    - RFC 1035
- Important name servers have multiple instances which serve traffic in
  round-robin - the ideas of "primary DNS" and "secondary DNS" don't really
  apply in the modern world
- The top 3 layers in the DNS tree have names
    ```
    root node -> top-level domains -> second-level domains -> other levels
    ```
- dot is a separator in a domain name
- the root node is (conceptually) an empty string so `www.example.com.` is
  conceptually `www.example.com.<empty-string>`

Types of TLD:

1. Generic TLDs (gTLDs) e.g. .com, .org, .net
2. Country code TLDs (ccTLDs) e.g. .uk, .nz
    - two letter code defined by ISO 3166

- Each node within DNS tree is assigned to an _authority_ (an org or person
  responsible for the management of that node)
- The authority can delegate authority for lower levels of that node
- ICANN has authority for the (singular) root node
- ICANN defines what gTLDs are available
- ICANN delegates authority for gTLDs parts of the root domain to _accredited
  registrars_
    - Typically a registrar will have delegated authority for more than just one
      part e.g. they can register domains in .com, .org, .net etc.
    - Currently ICANN has 2505 accredited registrars
    - https://www.icann.org/en/accredited-registrars
- ICANN deletes authority for ccTLDs to entities within each country
    - .nz -> https://internetnz.nz/
    - .au -> https://www.auda.org.au/
    - .ie -> https://www.weare.ie/
- ccTLDs usually also have a list of registrars that they delegate authority to
  register new domains to
    - they may have divisions within their own namespace - some ccTLDs organise
      by function e.g. `.com.au`, `.org.au`, others by geography `.ny.us`
    - usually ccTLD org will manage the nameservers itself
    - .nz has ~60 registrars in their list

If registrars forced you to use only their name servers when you registered a
domain then they would map neatly into the DNS hierarchy - they would be
responsible for all the domains they register.

But they allow you to set your name servers to anything you want so while the
registrar will always have "authority" for that domain they might not run any
name server for it - their only job is to tell their higher up org which name
servers are authoritative for the domain.

We think in domains but the software thinks in zones

The registrar is always responsible for proving the whois info about the domain
registration.

When you register a domain with a registrar, they usually also provide you name
servers which are authoritative for your domain they also tell ICANN/ccTLD org
that those servers are authoritative for the new domain even if you want ot use
your own name servers, you need to tell the registrar who then passes the
addresses of the nameservers up to the entity which allows them to be a
registrar in that hierarchy

Authority does not necessarily change with each new layer in a domain tree e.g.
if I register example.com then I can control everything underneath it e.g.
`foo.bar.baz.example.com` so the authority levels would be

    . = ICANN
    com = ICANN
    example = whichever accredited registrar I used
    baz = same as above
    bar =  same as above
    baz =  same as above

### root servers

- https://root-servers.org/
- 13 **logical** Internet root name servers worldwide operated by 12 independent
  operators.
- 1489 server instances as of Jan 2022
- they have a reserved domain name `root-servers.net.`
- each root server is actually multiple server instances which share a single IP
  address via Anycasting (implemented in BGP)
- DNS software can include the root hints file to bootstrap it's knowledge of
  the root servers
    - https://www.internic.net/domain/named.root
- 13 chosen because it allowed common root server queries to be answered within
  a single 512 byte UDP packet
    - book doesn't say how this actually worked
    - DNSSEC kinda breaks this anyway
- the 13 root servers get their data about gTLDs and ccTLDs from a private ICANN
  server via DNSSEC
    - this data is a list of the authoritative name servers for .com, .org, .nz
      etc.

### TLD servers

- ICANN has a theoretically contractually but in reality consultative
  relationship to the ccTLD operators
    - delegated by ICANN to a _country code manager_
- ICANN has a contractual relationship to the gTLD operators
- ICANN created two kinds of entity:
    1. Registry operators
        - contracted by ICANN to operate the name servers for a single gTLD
        - queries to the root domain return references to these name servers
        - registry operators get the list of registered domain names from one or
          more _Registrars_
        - the public has no contact with Registry operators
        - some Registry operators are also Registrars e.g. Verisign
        - takes EPP commands from Registrars and turns them into a new zone file
          for the TLD servers
    1. Registrars
        - contracted and accredited by ICANN to allow the public to register one
          or more gTLDs
        - these are the companies you deal with when you register a domain
        - they maintain your tech/admin contacts and sort out the billing
        - the registrar sends the following to the registry operator:
            1. the domain just registered
            2. the **IP addresses** of the authoritative name servers
        - provide a web ui to access the whois database
- information between Registry operator and Registrar controlled by Extensible
  Provisioning Protocol (EPP)
    - https://datatracker.ietf.org/doc/html/rfc5730
    - stateful XML protocol
    - Implementation: https://fred.nic.cz/en/

Q: who holds the source of truth of the whois database I think it is run by the
registry operator it must be, it wouldn't make sense to be registrars - too
fractured

A common sequence of finding a DNS answer

1. Enter url in browser
1. Browser searches internal cache, uses IP if found
1. Browser sends request to OS stub-resolver
    1. Stub resolver returns answer from cache if possible
    2. Stub resolver creates the DNS query
    3. Sends query to the configured resolver
        - this could be a DNS Proxy (e.g. a home router) or an actual resolver
          (e.g. 1.1.1.1)
        - the DNS proxy may also have a cache
        - there are no standards for DNS proxies - they may pass traffic
          unaltered or they might try to "help" and/or cache
1. DNS resolver gets the request
    - it will check cache first
        - depending on how busy the resolver is, this may be useful
    - DNS resolver sometimes called
        - "caching name server"
        - "recursive name server"
    - the resolver will issue a sequence of queries to chase down an
      authoritative answer to the query

Name servers support "zones" zone != domain

- a zone describes an operational and authoritative part of a domain name
  managed by a name server
- zone file format portable between implementations
- one zone file per managed domain

If a domain has multiple name servers, they may be configured as a mix of
masters and slaves (you can have multiple of each) This doesn't imply any
precedence in masters being preferable to get answers from

Zone files

- are actually standardised by RFC 1035
