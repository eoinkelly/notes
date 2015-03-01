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
