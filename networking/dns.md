# DNS records

* http://en.wikipedia.org/wiki/List_of_DNS_record_types

* each "resource record" (database row) is stored in a "zone file" and has the following fields

1. hostname
2. type
3. value
4. TTL

# @ symbol

* RFC 1035 defines @ as a shortcut for the "current origin"
* it is a shortcut for the root of your domain e.g. `@` is a shortcut for `foo.com` if you are configuring DNS records for `foo.com`

# Common DNS records

* A
    * address record
    * maps the string in "hostname" to the IP address in "value" field
* CNAME
    * canonical name record
    * aliase one hostname to another
    * if the "hostname" matches then retry the lookup with the string in "value" as a query
* TXT
    * arbitrary text. originally just for human readable text but often machine
      readable now e.g. Sender Policy Framework, keybase verification
* MX {number}
    * maps a domain to a list of "message transfer agents" (aka mail servers) for that domain
    * {number} is the priority of that MTA
* SRV {number}
    * Service locator
    * used for newer protocols instead of creating protocol specific records like MX
    * works like a generalized MX record

# dig

```
dig -t ANY interesting.com @some_name_server
dig -t ANY interesting.com @8.8.8.8
```

# ANAME or ALIAS records

Lenz says it breaks the internet https://iwantmyname.com/blog/2014/01/why-alias-type-records-break-the-internet.html

Needed by

1. Heroku
2. Azure

* CNAME records cannot be used on naked domains
    * The DNS spec says so
    * If you do it, it can break email services on your domain

* Heroku/Azure don't want to provide A records for naked domains because
    1. Scalablilty - they want to be able to move apps around on different
       servers to manage scaling them
    2. Security - if there was an attack on their servers, they want to be able
       to move apps around.
    The upshot is they can't provide a stable IP address for A record

Some DNS companies provide a "CNAME functionality on the zone apex" service

Downside: there are no official standards around ALIAS-type DNS records yet,
and there may be issues with general service availability, content delivery
networks and DNSSEC
* some domain extensions e.g. `.is` don't allow it

## Alternative 1: ALIAS records

> The right way of doing it is to accept that a root domain mapped onto another service either needs an IP address to point to or an HTTP redirect to a 'www' subdomain that can be pointed to your destination via CNAME.

> These ALIAS-type records blur the lines between a caching resolver and an authoritative nameserver. An ALIAS record resolves on request the IP address of the destination record and serves it as if it would be the IP address for the apex domain requested. If the IP address for the destination changes, the IP address for the mapped domain changes automatically as well.

An ALIAS record resolves _on request_ the IP address of the destination record

I ask "what is the IP address of "foo.com" (note I am asking for the IP of a domain, not a machine within that domain
1. The resolver looks up the current IP address of foo.com and returns it as the answer

## Alternative 2: URL forwarding

Forward the naked domain to the www subdomain using a HTTP 301 redirect -
iwantmyname.com call this the "web forwarding" service in their control panel

I think they implement this by making the A record of the domain point to their
"web forwarding" service.

* -- an extra HTTP redirect before the user first gets to your site
    * latency of a creating a tcp connection and the HTTP SYN, ACK , SYN-ACK and then response
* ++ more compliant with the DNS spec
* -- the naked domain (zone apex) only works for web traffic - other services will not resolve

Github pages have their own custom way of making naked domains work

