# dig

* uses nameservers in `/etc/resolv.conf` by default
* WARNING: does not use the macOS system DNS stuff so you might get different answers from dig and that
* `type` option
    * can be any valid query type supported by BIND9
    * `type` defaults to A record
    * types
        * A address records
        * MX mail records
        * ANY All records
        * PTR used for reverse lookup
        * SOA zone of authority (TODO: what is this)
* most useful query options
    * add `+norecurse` to prevent recursive lookups
    * add `+trace` to trace the recursive lookups
* default query class is `IN` (internet addresses)
    * other query classes
        * HS Hesiod record
        * CH Chaosnet record
* You can request zone transfer by setting type to AXFR
* You can request incremental zone transfer by setting type to IXFR

Examples

```
dig @<SERVER> <RESOURCE_RECORD_NAME> <QUERY_TYPE>
dig @8.8.8.8 foo.com -t any +trace
```

## Reverse lookups

To do a reverse lookup

```
dig @8.8.8.8 -x <IP_ADDRESS>
```

This automatically:

* converts the ip address from `A.B.C.D` to `D.C.B.A.in-addr.arpa`
* sets query type to PTR
* sets class to IN
