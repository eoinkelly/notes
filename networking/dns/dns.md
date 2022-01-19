# DNS records

All modern DNS servers fall into one of these roles:

1. Recursive resolvers
2. Root nameservers
3. TLD nameservers
4. Authoritative nameservers
5. Forwarding resolver (not really a DNS server, more of a caching DNS proxy)

These roles a bit fuzzy because a single piece of software can play multiple roles e.g. Bind

Terminology: Stub resolver

> In RFC 1034 terminology, a "stub resolver" is what is generally employed in
> the Unix and Linux world: a fairly dumb DNS client library running in the
> application processes, talking the same DNS/UDP and DNS/TCP protocols to an
> external program running as another process, that actually does the grunt work
> of query resolution by making back-end transactions and building up the
> front-end response from them.

## /etc/hosts (HOSTS.TXT) - the original DNS

* Througout the 70's HOSTS.TXT contained the `Name <-> IP` mapping of every domain on the Internet
* `/etc/hosts` was **built from** HOSTS.TXT -
    * a few times a week, admins would FTP the file from a known location and build `/etc/hosts` from it for their machine(s)
* problems with HOSTS.TXT
    * too much traffic to the central server hosting HOSTS.TXT
    * no two hosts could have the same name (NIC could assign addresses to guarantee uniqueness but it couldn't control hostnames so
    * consistency was getting too hard - changes came in frequently and it took a while for a new version of the file to propagate across the Internet

```
cat /etc/hosts
##
# Host Database
#
# localhost is used to configure the loopback interface
# when the system is booting.  Do not change this entry.
##
127.0.0.1  localhost
255.255.255.255  broadcasthost
::1             localhost
```

In 1984, RFCs 882 and 883 were published which described DNS (these were later superseded by 1034 and 1035)

DNS organised like a unix filesystem

* `/` is both the separator and the root node in filesystem.
    * Conceptually there is a "null label" or empty label e.g. `{null-label}/users/eoin` so the separator is just the separator
    * DNS has a similar design
    * the DNS null label is sometimes represented as `.` because (I guess) its a representation of `.{null-label}` e.g. `bar.foo.com.{null-label}` is actually written as `bar.foo.com.`
    * A domain name is a series of labels. This `{null-label}` design allows you to store te domain name as `{length_1}{label_1_chars}{length_2}{label_2_chars}...{zero byte}` (the length of the last label is 0) which happens to nicely correspond to how strings are represented in C.
* directory in filesystem => domain in DNS
* a directory can have subdirectories => a domain can have subdomains
* `/` is the separator in filesystem => `.` is the serparator in DNS
* NFS in filesystems is a bit like the distributed nature of DNS - the admin of an NFS mounted node gets to decide the heirarchy
* Domain names are used as indexes (or, more loosely, "search keys") into the DNS database
* you can think of all DNS data as "attached" to a hostname
* DNS can have 127 levels of domain label
* Each label can be up to 63 chars long
* The null (zero length) lable is reserved for the root
* `foo.bar.com.` actually has the separator and then the zero-width root node in it
* if the root node needs to be written by itself it appears as a `.` but don't confuse that with `.` being the separator (let's be honest, that's pretty fucking confusing)
* sometimes software interprets a trailing `.` on a domain name as indicating it is a FQDN
    * I _think_ BIND does this
    * most software does not seem to enforce this
* both hosts and domains are both represented by domain names
    * an individual server is represented as a domain name
    * all the resources in a university are represnted by a domain name
    * an "interior" domain name e.g. `bar.com.` in `foo.bar.com.` can represent both a host and a domain
* the words _domain_ and _subdomain_ are basically interchangable
* From RFC 1034:
    > By administrative fiat, we require every zone to be available on at least two servers
    * i.e. how many name servers your domain is replicated on is a convention not a technical requirement
* history
    * RFC 1034 talks about using DNS to store email addresses (replace the `@` with `.` and they look like a domain) but that never really took off

Anatomy

* domain name space
    * a tree structure
    * each node and leaf has a _set of resource records_ which may be empty
    * there is no distinction between interior nodes and leaf nodes
    * They deliberately designed so the name space did not have to be organised on the same lines as the network space
* node
    * a node on the tree
    * has a _resource set_ aka a set of resource records
* label
    * 0-63 chars in length (0 length label is reserved for the root node)
    * names can be stored in either upper or lowercase but all comparisons are case insensitive
    * case insenstive
    * can contain `a-zA-Z0-9-` only
    * must start with a letter `a-zA-Z`
    * must end with letter or digit
* domain name
    * the list of labels on the path from the node to the root of the tree
    * separated by `.` when represented as a string
    * domain names which end in `.` are taken to be absolute, otherwise the domain name is taken to be relative
    * max length 255 bytes
    * must be ASCII
* domain
    * identified by a _domain name_
    * is the part of the _domain name space_ which is at **or** below the domain name which specifies the domain
    * You can visualise a _domain_ as an area encompassing all domains and subdomains under a given domain label
* Aside: `ac` as in `.ac.uk` or `.ac.nz` stands for _academic community_

Anatomy of a query

RR = resource record

A query has 4 sections - they are:

1. Question
    * Carries the query name and other query parameters.
1. Answer
    * Carries RRs which directly answer the query.
1. Authority
    * Carries RRs which describe other authoritative servers.  May optionally carry the SOA RR for the authoritative data in the answer section.
1. Additional
    * Carries RRs which may be helpful in using the RRs in the other sections.

## Zone file

* A _zone_ is some part of the domain _name space_
* zone file is also called _db file_ or _database_ file
* zone files are a defacto standard because they are how bind loads zone data
* Difference between _domain_ and _zone_
    * You can visualise a _domain_ as an area encompassing all domains and subdomains under a given domain label
    * You can visualise a _zone_ as an area encompassing all domains and subdomains under a given domain label which have not been delegated away
    * i.e. zones are "all the domains under this domiain label which are managed by this server"
    * a domain can contain data managed by other (delegated) servers
* Management of zones can be delegated to other entities
* Plain text file of fields separated by whitespace (space or tab)
* Format described in RFC 1034/1035
* Global directives section at top of file
    * begin with `$`
    * Examples
        * `$ORIGIN example.com.`
            * sets the starting point for the zone in the DNS heirarchy
            * if missing this value is inferred by whatever is in the Server config file
        * `$TTL 300`
* Columns
    1. Name
        * can be blank - if blank it inherits from the previous record in the file
        * the meaning of `Name` depends on the record type e.g. `Name` means something different in a CNAME record vs an MX record
            * for _most_ record types, name is set to the `$ORIGIN`
    1. TTL
        * defaults to global TTL but it can be set here
    1. Record class
        * Usually this is `IN` for Internet but other kinds do exist
    1. Record type
        * Examples
            * SOA
                * lists
                    1. authoritive name server for the zone
                    1. the email address of somebody responsible for management of the zone (the `@` is replaced with a `.`)
                    1. Timing and expiriation params
                        *  (serial number, slave refresh period, slave retry time, slave expiration time, and the maximum time to cache the record)
            * A
            * MX
            * CNAME
            * etc.
    1. Record data
        * the meaning of the data depends on record type
        * if the data has multiple fields they are separated by whitespace
* Host names which do not end in `.` are relative to the `$ORIGIN` e.g. `www` as hostname is actually `www.example.com` in our example
  * host names ending in `.` are said to be _fully qualified_
* Minimum requirements
    * zone file must have a SOA record
* Format was originally used by BIND but other servers adopted it (some just use it as seed data from their internal DB)
* can either be authorative for a domain or just a listing of the cached DNS data
* `@` symbol
    * RFC 1035 defines @ as a shortcut for the "current origin"
    * it is a shortcut for the root of your domain e.g. `@` is a shortcut for `foo.com.` if you are configuring DNS records for `foo.com.`

https://en.wikipedia.org/wiki/Zone_file

```
$ORIGIN example.com.                          ; designates the start of this zone file in the namespace
$TTL 1h                                       ; default expiration time of all resource records without their own TTL value
example.com.  IN  SOA   ns.example.com. username.example.com. ( 2007120710 1d 2h 4w 1h )
example.com.  IN  NS    ns                    ; ns.example.com is a nameserver for example.com
example.com.  IN  NS    ns.somewhere.example. ; ns.somewhere.example is a backup nameserver for example.com
example.com.  IN  MX    10 mail.example.com.  ; mail.example.com is the mailserver for example.com
@             IN  MX    20 mail2.example.com. ; equivalent to above line, "@" represents zone origin
@             IN  MX    50 mail3              ; equivalent to above line, but using a relative host name
example.com.  IN  A     192.0.2.1             ; IPv4 address for example.com
              IN  AAAA  2001:db8:10::1        ; IPv6 address for example.com
ns            IN  A     192.0.2.2             ; IPv4 address for ns.example.com
              IN  AAAA  2001:db8:10::2        ; IPv6 address for ns.example.com
www           IN  CNAME example.com.          ; www.example.com is an alias for example.com
wwwtest       IN  CNAME www                   ; wwwtest.example.com is another alias for www.example.com
mail          IN  A     192.0.2.3             ; IPv4 address for mail.example.com
mail2         IN  A     192.0.2.4             ; IPv4 address for mail2.example.com
mail3         IN  A     192.0.2.5             ; IPv4 address for mail3.example.com
```

After "name expansion" (my phrase, not an official one) the first column expands to be a list of FQDNs

* `@` replaced with `example.com.`
* empty lines replaced with the value from the line above
* all other strings assumed to be relative to `example.com.`
becomes

```
$ORIGIN example.com.
$TTL 1h
example.com.         IN  SOA   ns.example.com. username.example.com. ( 2007120710 1d 2h 4w 1h )
example.com.         IN  NS    ns.example.com.
example.com.         IN  NS    ns.somewhere.example.
example.com.         IN  MX    10 mail.example.com.
example.com.         IN  MX    20 mail2.example.com.
example.com.         IN  MX    50 mail3.example.com.
example.com.         IN  A     192.0.2.1
example.com.         IN  AAAA  2001:db8:10::1
ns.example.com.      IN  A     192.0.2.2
ns.example.com.      IN  AAAA  2001:db8:10::2
www.example.com.     IN  CNAME example.com.
wwwtest.example.com. IN  CNAME www.example.com.
mail.example.com.    IN  A     192.0.2.3
mail2.example.com.   IN  A     192.0.2.4
mail3.example.com.   IN  A     192.0.2.5
```

Note

* A client can send queries based on the first 3 columns (name, class, type)
* The 4th column (record data) is treated like an opaque string value (you cannot search on it)
* A server will augment the (name, class, type, data) tuples from its zone files with things it learns and caches from other servers

querying is not "heirarchical" in any way - you are just querying for a matching 'name' string - there is no "send me all records under example.com.", instead you are saying "send me all records with 'example.com.' in the name field

If a server thinks it is authoritive for example.com. then

* it expects to have all the records that exist with name `example.com.`
* it expects to hae **all** records for subdomains **directly** under `example.com.`
* it expects to have NS records for any delegated subdomains **directly** under `example.com.`
* it expects to have NS records for any servers which have a delegates subdomain directly under `example.com.`

## How delegation works

Imagine we have `foo.com.` and `bar.foo.com.` is delegated to another server

1. Recursive (resolving) server asks for an A record for `bar.foo.com.`
1. The authoritive nameserver for `foo.com.` returns NS records for `bar.foo.com.` which tells the resolving server that it needs to go talk to that NS server
1. The authoritive NS for `bar.foo.com.` returns an A record

## Software

Two kinds of server "role"

1. Recursive server role
    * name servers can do "resolution" because many resolvers are not smart i.e. name servers can help you search through the domain name space
1. Authoritive server role
    * An authoritive nameserver has **complete** information for its **zone** (not the same as domain)
        * Authorative information is organised into zones not domains
    * primary name server for a zone reads value from the zone file on disk
    * secondary name servers do a zone transfer from the primary when the start up
    * both primaries and secondaries are authoritive for the zone
    * zone:nameserver is many:many
        * a name server can be authoritive for more than one zone
        * a zone can (and usually does) have more than one authoritive name server

Important features

* Split horizon feature
    * can give different answers depending on the source IP of the query
* Wildcard
    * can publish information for wildcard records, which provide data about DNS names in DNS zones that are not specifically listed in the zone

### DNSmasq

* A dns forwarder, DHCP and TFTP server for small scale networks
* will load `/etc/hosts` to server the names of local machines which are not in DNS

### BIND

```
# on ubuntu
sudo apt install bind9 bind9-doc bind9utils
# config is in /etc/bind
```

* The first implementation of DNS was called JEEVES, BIND came later as part of BSD unix.
* BIND is the most popular DNS server today
* runs as `named` on a system
* BIND 9 (released 2000)
    * is a ground-up rewrite.
    * All older versions are obsolete
    * supports DNSSEC
    * has a web and command line interface
* 9.4 added the ability to store zone data in formats other than flat text file i.e. LDAP, Berkeley DB, PostgreSQL, MySQL, and ODBC.
* has the following components
    1. name server
    1. lightweight resolver
    1. name server tools:
        * dig
            * allows users to run queries
        * host
            * convert hostnames to IPs
        * nslookup
            * queries DNS servers for info about hosts **and** domains
        * rdnc (Remote name daemon control) - lets admin control the server remotely over encrypted channel
* BIND 10 exists but work has stopped since 2014 because ISC does not have the resources to do both BIND9 and BIND10
    * seems like it was a DHCP and DNS server and it is basically dead now
* Runs on all major unixen and Windows

## TTL

> Recursive DNS servers, whether open recursives or local resolvers, typically do not acknowledge a TTL of less than 30 seconds.

http://www.zytrax.com/books/dns/info/minimum-ttl.html
> The author of this note considers low - sub one minute - TTLs to be inherently evil for 3 reasons

* Minimum TTL cloudflare allows is 2 mins
* Cloudflare "Automatic" TTL is approx 5 mins (300 sec) https://support.cloudflare.com/hc/en-us/articles/360017421192-Cloudflare-DNS-FAQ#whatdoestheautomaticttlvaluemean - I don't know why they say "approximately" - maybe the tweak it based on some condition?

> the lowest TTL in DNS Made Easy is 30 seconds. That’s because resolving name servers will usually only pay attention to TTL’s that are 30 seconds or highers

Longer TTLs
    * reduce cost if you are paying by the lookup to your authoritive name server
    * improve performance for users because they get their answer quicker

Layers of caching

1. In the browser itself
    * `getaddrinfo()` does not include a TTL so the browser never knows how long the TTL was on a record so it cannot honor it. It will cache DNS results for some period of time it chooses
    * Old browsers cached for much longer - apparently old IE cached for 30 mins but newer browsers cache for 3 mins (this info may be out of date)
1. In the resolver on the host the browser is running on
1. DNS resolvers which answer queries for the host
1. The authoritive name server

## CNAME flattening

* Cloudflare does this

When a query comes in for the root domain and you have configured a CNAME for that root domain then cloudflare's authoritive name server will act as a resolver and follow the CNAME change until it gets and IP address - it will return that IP as an A record in the query answer.

## Common DNS records

DNS RR (Resourc records) have the following form:

    {owner_domain} {class} {type} {ttl} {rdata}

* `{owner_domain}` is the domain which owns this record
* `{class}` two classes available: `IN` (Internet system, the one we actually use), `CH` (Chaos system, very uncommon these days)
* `{type}` 16 bit value representing the type e.g. A, CNAME, MX, NS etc.
* `{ttl}` 32 bit int representing number of seconds that resolvers should keep this record in cache (TTL does not apply to authorative servers)
* `{rdata}` depends on the type, somtimes it is a binary string, sometimes a "domain name" (which I think is a particular format of null terminated string where the labels are prefixed by lengths)
    * when it is a "domain name" type, then the value is often a pointer to other domains within the system

The order of RRs i a domain is not relevant or maintained by the software.

http://en.wikipedia.org/wiki/List_of_DNS_record_types

* A
    * Fields
        * Name: `{host-name}`
            * if not a fully qualified domain name then it is relative to the `$ORIGIN`
            * Can use `@` as an alias for `$ORIGIN`
        * Record data: `{a-single-ipv4-or-ipv6}`
    * Purpose: maps the string in "hostname" to the IP address in "value" field
* CNAME
    * Fields
        * Record name field: `{the-alias}`
        * Record data field: `{fqdn-we-are-pointing-to}`
    * Purpose: alias one domain name to another
    * Many systems have the notion of a "canonical name" for a resource which can have many aliases i.e. there is one name that is singled out and treated as "special" and dubbed canonical.
    * A CNAME record idenifies its `{owner_domain}` as a _non canonical alias_ for the domain listed in the `{rdata}` part of the record.
    * **If a node has a CNAME then it can't have any other kind of record** because
        1. It ensures that the aliases and the canonical name node cannot have different data
        1. It ensures that a cached CNAME can be used without checking back with the authoritive server for other RR types
    * CNAME RRs cause special action in DNS software.
        * When a name server fails to find a desired RR in the resource set associated with the domain name, it checks to see if the resource set consists of a CNAME record with a matching class.  If so, the name server includes the CNAME record in the response and restarts the query at the domain name specified in the data field of the CNAME record.
        * The one exception to this rule is that queries which match the CNAME type are not restarted.
* TXT
    * Fields
        * Name: `$ORIGIN`
        * Record data: `"{any-string}"`
            * each line must be encosed in double quotes
            * a common max single record length seems to be 255 chars in most implementations
    * arbitrary text originally just for human readable text but often machine
      readable now e.g. Sender Policy Framework, keybase verification
* MX
    * Fields
        * Name: `$ORIGIN`
        * Record data: `{number} {mail_server_domain_name}`
    * maps a domain to a list of "message transfer agents" (aka mail servers) for that domain
    * {number} is the priority of that MTA
* SOA
    * Fields
        * Name: `$ORIGIN`
        * Record data: `{fqdn-to-primary-name-server} {email-addr-of-responsible-person} {a} {b} {c} {d} {e}`
            * The email address has `@` replaced with `.` e.g. `miles.obrien.gmail.com`
            * Numbers
                1. `{a}` serial number
                1. `{b}` slave refresh period
                1. `{c}` slave retry time
                1. `{d}` slave expiration time
                1. `{e}` the maximum time to cache the record
    * This record asserts which name server is authoritive for the given domain
    * Example:
        ```
        ackama.com.		900	IN	SOA	ns-1513.awsdns-61.org. awsdns-hostmaster.amazon.com. 1 7200 900 1209600 86400
        ```

* NS
    * Fields
        * Name: `$ORIGIN`
        * Record data: `{fqdn-of-ns-server}`
            * Data can have multiple server fqdn separated by newline
* SRV {number}
    * https://tools.ietf.org/html/rfc2782
    * `_service._proto.name. TTL class SRV priority weight port target.`
  * `_sip._tcp.example.com. 86400 IN SRV 0 5 5060 sipserver.example.com.`
    * Service locator
    * used for newer protocols instead of creating protocol specific records like MX
    * works like a generalized MX record
    * Fields
        * Name: `_{service}._{proto}.{name}.`
        * Record data: `{TTL} {class} {SRV} {priority} {weight} {port} {target}.`
* ALIAS
    * depends a lot on the authoritive server - not an official part of DNS spec
    * these look like `A` records to the querying client so the authtoritive name server **must be able to resolve them to an IP address** because that is what it needs to return to queries
    * Fields
        * Name: `$ORIGIN`
        * Record data: `ALIAS some-elb-yoke.ap-southeast-2.elb.amazonaws.com.`
* SPF
    * https://datatracker.ietf.org/doc/rfc4408/
* PTR
  * Pointer
* AAAA
  * IPv6 address
* NAPTR
  * Name authority pointer
* CAA
  * Certificate authority authorization
* DS
* RRSIG

### ANAME or ALIAS records in detail

* aka ANAME’s, root CNAME’s, root domain redirect, zone apex CNAME’s
* `ALIAS` records are presented to clients/resolvers as `A` records
* Are not part of DNS spec: https://iwantmyname.com/blog/2014/01/why-alias-type-records-break-the-internet.html
* Some DNS providers call their solution to "no CNAME to root" something other than ALIAS e.g.
    * Virtual CNAME at cloudflare
    * ANAME at DNS made easy

Are needed by any service where the IP address of the service might change e.g.

1. Heroku
2. Azure
3. AWS

Heroku/Azure/AWS don't want to provide A records for naked domains because

1. Scalablilty - they want to be able to move apps around on different servers to manage scaling them
2. Security - if there was an attack on their servers, they want to be able to move apps around.

The upshot is they can't provide a stable IP address for A record

#### Why not use CNAME on naked domain?

* CNAME records cannot be used on naked domains
    * The DNS spec says so
    * If you do it, it can break email services on your domain

> What’s important here is that this is a contractual limitation, not a
> technical one. It is possible to use a CNAME at the root, but it can result
> in unexpected errors, as it is breaking the expected contract of behavior.
>
> https://medium.freecodecamp.org/why-cant-a-domain-s-root-be-a-cname-8cbab38e5f5c

#### Downsides of ALIAS

* there are no official standards around ALIAS-type DNS records yet
* there may be "issues with general service availability, content delivery networks and DNSSEC"
    * TODO: what does this mean? ALIAS records are super common in 2018
* some domain extensions e.g. `.is` don't allow ALIAS records

> These ALIAS-type records blur the lines between a caching resolver and an
> authoritative nameserver. An ALIAS record resolves on request the IP address
> of the destination record and serves it as if it would be the IP address for
> the apex domain requested. If the IP address for the destination changes, the
> IP address for the mapped domain changes automatically as well.

How `ALIAS` record works


An ALIAS record resolves _on request_ the IP address of the destination record

1. Resolver asks "what is the A record (aka IP address) of `foo.com.` (note I am asking for the IP of a domain, **not** a machine within that domain)
1. Authorative name server looks up the current IP address of foo.com and returns it as the `A` record to the resolver

Note

* The authoritive server became a resolver when you asked it for the `A` record for the domain - it turned into a resolver and found the answer and then returned it to you as an `A` record
* It is entirely up to the authoritive server **how** it should discover the IP address of `foo.com` and **how long** it should cache that value for.

#### Alternative to ALIAS records: URL forwarding

Forward the naked domain to the www subdomain using a HTTP 301 redirect - iwantmyname.com call this the "web forwarding" service in their control panel

I think they implement this by making the A record of the domain point to their "web forwarding" service.

Pros/cons

* -- an extra HTTP redirect before the user first gets to your site
    * latency of a creating a tcp connection and the HTTP SYN, ACK , SYN-ACK and then response
* ++ more compliant with the DNS spec
* -- the naked domain (zone apex) only works for web traffic - other services will not resolve

## Query types

1. Recursive query
    * asks the server to send as many queries as required to find your answer
    * `dig` sends recursive queries by default. `dig +recurse` to be explicit
    * BIND nameserver can be configured to reject recursive queries
1. Iterative query
    * lets the server send back the best answer it knows to the queryer
    * `dig +norecurse` to send iterative queries
    * `dig +trace` and `dig +nssearch` options will also use iterative queries
    * when a nameserver receives an iterative query it will do its best to return a helpful response e.g. it will try to find the NS servers which are closest to what the query asked for

## Name to address mapping

1. You app calls libc `getaddrinfo` http://man7.org/linux/man-pages/man3/getaddrinfo.3.html to ask for the IP address(es in linked list) that corresponds to the given hostname
    * Chrome (and maybe other browsers) have their own networking stack so maybe they skip this?
    * Most resolver libs are quite "stupid" and must use a recursive query to get an answer because they can't handle doing it iteratively
1. `getaddrinfo` asks the operating system "resolver" to do the lookup
    * Linux/unix
        * looks for resolver IP addresses listed in `/etc/resolv.conf`
    * macOS
        * `scutil --dns` to inspect DNS configuration
        * resolv.conf exists but its autogenerated
        * my mac laptop has a resolv.conf of
            ```
            search lan
            nameserver 192.168.1.1
            ```
    * Windows
        * ?
1. The resolver sends an "recursive" DNS query to first server in `/etc/resolv.conf` (or whatever list the OS uses)
    * resolvers are not smart
    * recursive queries put most of the work on the server which receives them - they tell the server that it should talk to as many nameservers as necessary to find an answer
    * resolver DNS servers are servers which will do lookups for clients
    * bind can be configured to be a resolving server, `dnsmasq` is another common linux option
    * examples
        * on a home network, the broadband modem is often also a resolver
1. That DNS server behaves as a "resolving server" and either
    1. answer the query from its cache if possible
    1. answer the query directly if it is authoritive for the domain being queried (this is unlikely in most cases)
    1. do a query to the authoritive server


How a resolver answers a recursive query:

The resolving server you talk to **does all the work** of talking to as many name servers as necessary to find your answer. Resolving server implementations are not required to do this but most do out of "politeness" :-)

Say we receive a query for A records on `www.foo.com`:

1. Look up our internal, hard-coded set of root server IP addresses and choose one at random.
1. Send the query we recieved to the root server
1. The root server response
    * ANSWER section: empty because it doesn't know the answer to our query
    * AUTHORITY section: the names of server authoritive for `.com`
    * ADDITIONAL section: the IP addresses of the servers mentioned in the AUTHORITY section
1. Choose a server from the AUTHORITY section above and send it the exact query we originally received
1. The .com authoritive server response will contain:
    * ANSWER section: empty because it doesn't know the answer to our query
    * AUTHORITY section: the names of server authoritive for `foo.com`
    * ADDITIONAL section: the IP addresses of the servers mentioned in the AUTHORITY section
1. Choose a server from the AUTHORITY section above and send it the exact query we originally received
1. The foo.com authoritive server response will contain:
    * ANSWER section: contains the answer we want
    * AUTHORITY section: the names of server authoritive for `foo.com` (not that useful to us anymore)
    * ADDITIONAL section: empty
1. Store the answer in a cache to speed up future requests
1. Send the answer back to the client

Note that the resolver

* naievely asked **every** server along the way about A records on `www.foo.com` - the resolver never split the query into labels
    * splitting into labels and trying to be smart about it could bake in an assumption that zones of authority match the heirarch exactly which isn't true e.g. given a query `waxwing.ce.berkeley.edu` we cannot depend on there being a separate authoritive server at each level e.g. `{root}`, `edu`, `berkeley`, `ce`, `waxwing`. The reality is that the zones are probably organised as `{root}`, `edu`, `berkeley`
* isn't really very smart - it puts the work on the receiving authoritive server to parse our query and send back a useful next step in the AUTHORITY section


## Address to name mapping

Uses

* showing more useful stuff in log files
* some authorization checks (.rhosts, hosts.equiv) (Aside: this seems like a feckin' tip-top security choice ...)

Overview

* the _domain space_ is indexed by name only so addresses are found by creating a special part of the _domain space_ where "names" represent addresses.
    * this is the `.in-addr.arpa` part of the _domain space_
    * numbers are in reverse order e.g. `100.101.102.103` would be `103.102.101.100.in-addr.arpa` because
        1. IP address get more specific as you go left to right and domain names get less specific as you go left to right
        2. This scheme makes it practical to delegate responsiblity for "names" in a way that makes sense e.g. if the IP address octets in the `.in-addr.arpa` domain were in the correct order then NIC would have to try and delegate all IP addresses which contain a given number in the first octet e.g. you would have to delegate all IP addresses which begin with `100` to somebody - basically impossible
* dig can do the lookup without you having to reverse the octets e.g. `dig -x 100.101.102.103`
* => the people who run various IP subnets

## IQUERY or Inverse queries (of historical interest only)

* Obsoleted by RFC 3425 in 2002 https://tools.ietf.org/html/rfc3425
* the query is processed by a single server only! the server will not recursively do an inverse query
* the search places a lot of demand on a given server
* the server searches all its data for the given query
* implementation of inverse queries is optional in the spec
* bind 8 recognises inverse queries will make up fake responses for the query

## Caching

* BIND implements both
    * positive caching (answers are remembered)
    * negative caching (the lack of an answer is remembered)
* The zone file requests how long various bits of data should be remembered for by setting TTL

Aside: TTL and Route53

* longer TTL values in a service like Route53 mean you pay less for it because the root name servers get queried less often
* AWS recommends a TTL of 60 sec if your record depends on a health check so that it will respond quickly


    A given name server will typically support one or more zones, but this
    gives it authoritative information about only a small section of the
    domain tree.  It may also have some cached non-authoritative data about
    other parts of the tree.  The name server marks its responses to queries
    so that the requester can tell whether the response comes from
    authoritative data or not.

    How is this visible in dig?


# EDNS

* EDNS0 is the first approved set of mechanisms for DNS extensions
* RFC: https://tools.ietf.org/html/rfc6891
* Extension Mechanisms for DNS were specified in 1999 (https://tools.ietf.org/html/rfc2671), with a minor update in 2013 (https://tools.ietf.org/html/rfc6891)
* Supported by cloudflare and Route53
* It looks like basically everything supports EDNS0 now
    * https://dnsflagday.net/ was a promise to stop working around DNS servers which didn't support it

# DNS server software

https://en.wikipedia.org/wiki/Comparison_of_DNS_server_software

* Bind
    * The most widely deployed
* Dnsmasq
    * not a real recursive DNS server
    * is more of a "DNS forwarder" i.e. will attempt to answer out of `/etc/hosts` and then forward the query to a real recursive server
    * is a kind of small network swiss-army knife. It does DNS forwarding, can be an authoritive server for your local network, is a DHCP server

UP TO: RFC 1034 section 3.7
UP TO 2.7 CACHING in DNS & Bind
