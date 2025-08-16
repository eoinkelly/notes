# getaddrinfo()

- `man getaddrinfo`
    - says nothing about _how_ it works
- part of POSIX.1 spec
- does a **lot** https://jameshfisher.com/2018/02/03/what-does-getaddrinfo-do/
    - it operates differently on different operating systems
- it does a DNS request as **part** of it's operation but it will also
    - read /etc/hosts
    - try to use nscd (a name lookup cache server) if it exists (don't install
      it, it's bit shit)
    - try to use mDNS if it exists (somehow??)
- is blocking (be careful calling if you are in a green threads context)
- part of libc

## caching

It's a single API endpoint which can query a number of systems, each of which
can do their own kinds of caching so it's hard to say whether it caches or not.

The general advice seems to be to treat `getaddrinfo` as if it does not cache
but on macOS at least, it's driven by the mDNSResponder process which does cache
answers e.g. the recommended way to flush your local DNS cache in macOS is to
send HUP signal to mDNSResponser

```bash
$ sudo killall -HUP mDNSResponder
```

Underlying systems:

- DNS
- mDNS
- /etc/hosts

## macOS

`/etc/resolv.conf` exists for compatibility but is not authoritative

```
scutil # tool to manage system configuration parameters
dns-sd # Multicast DNS (mDNS) & DNS Service Discovery (DNS-SD) Test Tool

dns-sd -q ackama.com a in # mimic's dig (but not as good)

scutil --dns # show current DNS configuration

❯ scutil --dns
DNS configuration

resolver #1
  search domain[0] : house
  nameserver[0] : 192.168.1.1
  if_index : 26 (en8)
  flags    : Request A records
  reach    : 0x00020002 (Reachable,Directly Reachable Address)

resolver #2
  domain   : local
  options  : mdns
  timeout  : 5
  flags    : Request A records
  reach    : 0x00000000 (Not Reachable)
  order    : 300000

resolver #3
  domain   : 254.169.in-addr.arpa
  options  : mdns
  timeout  : 5
  flags    : Request A records
  reach    : 0x00000000 (Not Reachable)
  order    : 300200

resolver #4
  domain   : 8.e.f.ip6.arpa
  options  : mdns
  timeout  : 5
  flags    : Request A records
  reach    : 0x00000000 (Not Reachable)
  order    : 300400

resolver #5
  domain   : 9.e.f.ip6.arpa
  options  : mdns
  timeout  : 5
  flags    : Request A records
  reach    : 0x00000000 (Not Reachable)
  order    : 300600

resolver #6
  domain   : a.e.f.ip6.arpa
  options  : mdns
  timeout  : 5
  flags    : Request A records
  reach    : 0x00000000 (Not Reachable)
  order    : 300800

resolver #7
  domain   : b.e.f.ip6.arpa
  options  : mdns
  timeout  : 5
  flags    : Request A records
  reach    : 0x00000000 (Not Reachable)
  order    : 301000

DNS configuration (for scoped queries)

resolver #1
  search domain[0] : house
  nameserver[0] : 192.168.1.1
  if_index : 26 (en8)
  flags    : Scoped, Request A records
  reach    : 0x00020002 (Reachable,Directly Reachable Address)

resolver #2
  search domain[0] : house
  nameserver[0] : 192.168.1.1
  if_index : 14 (en0)
  flags    : Scoped, Request A records
  reach    : 0x00020002 (Reachable,Directly Reachable Address)

```
