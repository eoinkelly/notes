# dig

* Apparently stands for _Domain information groper_
* WARNING: does not use the macOS system DNS stuff so you might get different answers from dig and that
* uses nameservers in `/etc/resolv.conf` by default
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

Aside: `dig +trace` uses your local resolver to ask it the address of the root servers - `dig` doesn't seem to have root servers hard-coded in

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

```
dig -t ANY interesting.com @some_name_server
dig -t ANY interesting.com @8.8.8.8
```

## TTL values in dig responses

Sometimes `dig` responses contain the TTL in the answer section, sometimes they do not. why?

If the server is authoritive, the number is the raw TTL of the record
If the server is not authoritive, it shows time remaining until the next refresh. You will get a different answer on the next request as the counter counts down
Note that for servers where one IP is multiple servers e.g. `@8.8.8.8` then you'll get the time of the server you are talking to so might get a different answer on the next request.

```
$ dig www.cst.cam.ac.uk

; <<>> DiG 9.10.6 <<>> www.cst.cam.ac.uk
;; global options: +cmd
;; Got answer:
;; ->>HEADER<<- opcode: QUERY, status: NOERROR, id: 64631
;; flags: qr rd ra; QUERY: 1, ANSWER: 3, AUTHORITY: 0, ADDITIONAL: 1

;; OPT PSEUDOSECTION:
; EDNS: version: 0, flags:; udp: 4096
;; QUESTION SECTION:
;www.cst.cam.ac.uk.		IN	A

;; ANSWER SECTION:
www.cst.cam.ac.uk.	21593	IN	CNAME	dept1-live.drupal.uis.cam.ac.uk.
dept1-live.drupal.uis.cam.ac.uk. 3593 IN CNAME	tm-128-232-132-21.tm.uis.cam.ac.uk.
tm-128-232-132-21.tm.uis.cam.ac.uk. 3593 IN A	128.232.132.21

;; Query time: 50 msec
;; SERVER: 192.168.1.1#53(192.168.1.1)
;; WHEN: Thu Nov 26 19:29:04 NZDT 2020
;; MSG SIZE  rcvd: 155
```
