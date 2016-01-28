# IPv6

## Sources

* https://en.wikipedia.org/wiki/IPv6_address
* defined mostly in RFCs 2460 - 2367 but there are quite a few others
    * there are many RFCs and they have many versions - watch out for the "obsoleted by" field on first page
    * header format defined in [RFC 2460](https://www.rfc-editor.org/rfc/rfc2460.txt)
    * address format defined in [RFC 4291](https://www.rfc-editor.org/rfc/rfc4291.txt)
    * rules for shortening addresses defined in [RFC 5952](https://www.rfc-editor.org/rfc/rfc5952.txt)

## General

* uses 128 bit (16 byte) IP addresses
    * not all addresses are available
        * some are reserved for future use
        * some are forbidden
* not backwards compatible with ipv4 - it essentially creates a "new internet" that sits beside the old one
    * moving traffic between and IPv4 and IPv6 network requires a gateway using one of the available translation mechanisms
        * translation mechanisms
            * NAT64
            * tunnelling
                * 6to4
                * 6in4
                * Teredo
* support in NZ
    * A table on NZ ISP support for it https://docs.google.com/spreadsheets/d/1KmhVGN_vPzerUzhpya8lBxdgHc1rCyuWIsYFNdW8wJU/edit#gid=0
        * Jan 2016 status: none of the big ones support it
* has a new IP packet header format
    * designed to minimize packet header processing by routers
* Only higher up layer protocols that embed IP addresses in their payloads need to change for IPv6
    * examples that do change
        * FTP
        * NTP
* multicasting is part of the base specification of IPv6 (unlike IPv4)
    * IPv6 has no special broadcast addresses
    * > In IPv6 you send a packet to the link-local all nodes multicast group at address ff02::1, which is analogous to IPv4 multicasting to address 224.0.0.1
* summary of technical benefits of IPv6
    * larger address space
    * permits heirarchical address allocation methods
        * helps with route agreement and so helps limit the expansion of routing tables
        * TODO:
    * use of multicast addresses is expanded and simplified
        * TODO:
    * "device mobility, security and configuration were considered in the design of the protocol"
        * very vague, TODO:
        * it includes IPSec
* apparently there are some new services being delivered via IPv6 only
    * examples: ???
* has solid OS support now - see https://en.wikipedia.org/wiki/Comparison_of_IPv6_support_in_operating_systems

# IPv6 network interface ususally has more than one IPv6 address

* IPv6-enabled network interfaces usually have more than one IPv6 address, for
  example,
    1. a link-local address
    2. global address
    3. permanent versus temporary addresses
* IPv6 introduces the concepts of address scope and selection preference,
  yielding multiple choices for source and destination address selections in
  communication with another host.
* The preference selection algorithm, which selects the most appropriate
  address to use in communications with a particular destination (including the
  use of IPv4-mapped addresses in dual-stack implementations), is based on a
  user-customizable preference table that associates each routing prefix with a
  precedence level.

# Address format

* addresses are 8x4 hex digits

IPv6 addresses are represented as eight groups of four hexadecimal digits with
the groups being separated by colons

    # example IPv6 address
    2001:0db8:0000:0042:0000:8a2e:0370:7334

    2001:0db8:0000  :0042      :0000:8a2e:0370:7334
    [network prefix][subnet id][Interface ID]
    [    routing prefix       ][

In the above example we see a

* 48 bit network prefix
* 16 bit subnet ID
* 64 bit Interface/Device ID


    address/prefix_size
    2001:db8:0:0:0:ff00:42:8329/64
    ::1/128

## Classification of addresses

NB: remember that an IPv6 network interface can have multiple addresses associated with it

There are three IPv6 address classes

1. Unicast
    * a packet sent to unicast address is delivered to a single network interface
2. Anycast
    * an anycast address is assigned to a group of interfaces (usually on different nodes)
    * a packet sent to an anycast address is sent to only one of the interfaces, usually whichever one is "nearest" as defined by the routing protocol
    * almost any unicast address can be used as an anycast address
3. Multicast
    * a packet sent to a multicast address is sent to all interfaces that have joined that multicast group on the router
    * broadcast is implemented as an "all nodes" multicast group denoted by `ff02::1`

Each type has a different way of grouping the bits in the 128 bit address

* Unicast
    * has two address formats:
        1. General unicast address format
        2. Link-local address format
    * General Unicast address format
        * fixed at 64 bits for "network prefix"
            * 48 or more bits: "routing prefix"
                * common routing prefix lengths: 48, 56, 64
            * 16 or less bits: "subnet id"
        * fixed 64 bits for "interface identifer"
            * can be
                * be generated from MAC address via "modified EIU-64" algorithm
                * obtained from a DHCPv6 server
                * assigned manually
                * generated randomly
    * Link local address format
        * bit fields
            * 10 bit prefix that is always FE80
            * 54 bit all zeros
            * 64 bit interface identifier
        * Implications
            * the network prefix is always the same for link-local addresses i.e. `fe80::/64` (<-- refers to just the network bit similar to how IPv4 might use 192.168.0.0/16
        * `fe80::/10`
            * This is a link-local prefix offered by IPv6.
            * This address prefix signifies that the address is valid only in the local physical link.
* Multicast
    * bit fields
        * 8 bit prefix, always FF
        * 4 bit flags
        * 4 bit scope
        * 112 bit group id
    * implications
        * any IPv6 address that starts with `FF` is a multicast address
    * `ff00::/8`
        * This prefix is offered by IPv6 to denote the multicast addresses.
        * Any address carrying this prefix is automatically understood to be a multicast address.

### Address abbreviation

For convenience, an IPv6 address may be abbreviated to shorter notations by application of the following rules, where possible.

1. One or more leading zeroes from any groups of hexadecimal digits are removed;
    * this is usually done to either all or none of the leading zeroes. For example, the group 0042 is converted to 42.
1. Consecutive sections of zeroes are replaced with a double colon (::).
    * The double colon may only be used once in an address, as multiple use would render the address indeterminate.
    * RFC 5952 recommends that a double colon must not be used to denote an omitted single section of zeroes.[45]
    * The two colons tell the operating system that everything in between them is a zero

An example of application of these rules:

Initial address:

    2001:0db8:0000:0000:0000:ff00:0042:8329

After removing all leading zeroes in each group:

    2001:db8:0:0:0:ff00:42:8329

After omitting consecutive sections of zeroes:

    2001:db8::ff00:42:8329

* The "loopback address", `0000:0000:0000:0000:0000:0000:0000:0001` may be abbreviated to `::1` by using both rules
* The "unspecified address", `0000:0000:0000:0000:0000:0000:0000:0000` may be abbreviated to `::` by using both rules


    # all these are the same address
    2001:cdba:0000:0000:0000:0000:3257:9652
    2001:cdba:0:0:0:0:3257:9652
    2001:cdba::3257:9652

### Zone index

* link-local addresses have a common prefix so normal routing procedures cannot be used to choose which outgoing interface to use to send the packets to a link-local destination
* the _zone index_ provides additional routing information which resolves this
    * in the case of link-local addresses the zone index corresponds to the network interface identifiers
* zone index is appened to the address separated by a `%`
* the format of zone index depends on OS

    fe80::8638:35ff:fe4d:9c48%1     # Windows style (zone index uses interface number)
    fe80::8638:35ff:fe4d:9c48%en0   # MacOS style (zone index uses interface name)
    fe80::8638:35ff:fe4d:9c48%eth1  # linux style (zone index uses interface name)

## Header format

* The IPv6 header is a standardised format, simplified by allowing headers to be chained together.
* There are only six fields, the two 128 bit addresses for source and destination, and no options.
* Simpler format than IPv4

No NAT is required ???


## The Loopback Address

The unicast address 0:0:0:0:0:0:0:1 is called the loopback address.  It may be
used by a node to send an IPv6 datagram to itself.  It may never be assigned to
any interface.

The loopback address must not be used as the source address in IPv6 datagrams
that are sent outside of a single node.  An IPv6 datagram with a destination
address of loopback must never be sent outside of a single node.

## The unspecified address

The address 0:0:0:0:0:0:0:0 is called the unspecified address.  It must never
be assigned to any node.  It indicates the absence of an address.  One example
of its use is in the Source Address field of any IPv6 datagrams sent by an
initializing host before it has learned its own address.

The unspecified address must not be used as the destination address of IPv6
datagrams or in IPv6 Routing Headers.

### IPv4 0.0.0.0

* In the context of servers, 0.0.0.0 means "all IPv4 addresses on the local machine"
* In the context of routing, 0.0.0.0 usually means the default route, i.e. the
  route which leads to "the rest of" the internet instead of somewhere on the
  local network.

## Literal IPv6 addresses in HTTP URLs and URIs

* Colon (:) characters in IPv6 addresses may conflict with the established
  syntax of resource identifiers, such as URIs and URLs.
* The colon has traditionally been used to terminate the host path before a
  port number.
* To alleviate this conflict, literal IPv6 addresses are enclosed in square
  brackets in such resource identifiers, for example:

    http://[2001:db8:85a3:8d3:1319:8a2e:370:7348]/

When the URL also contains a port number the notation is:

    https://[2001:db8:85a3:8d3:1319:8a2e:370:7348]:443/

Another example
    http://[::1]:8080/

    [::]:80 is [0000:0000:0000:0000]:80

# IPv6 DNS

* IPv6 uses AAAA resource records, which are sometimes referred to as Quad A records.
* The domain ip6.arpa is used for reverse hostname resolution.


# ifconfig examples

```
# mac example (notice the zone index suffix on the IPv6 address)
lo0: flags=8049<UP,LOOPBACK,RUNNING,MULTICAST> mtu 16384
	options=3<RXCSUM,TXCSUM>
	inet6 ::1 prefixlen 128
	inet 127.0.0.1 netmask 0xff000000
	inet6 fe80::1%lo0 prefixlen 64 scopeid 0x1
	nd6 options=1<PERFORMNUD>
en0: flags=8863<UP,BROADCAST,SMART,RUNNING,SIMPLEX,MULTICAST> mtu 1500
	ether 84:38:35:4d:9c:48
	inet6 fe80::8638:35ff:fe4d:9c48%en0 prefixlen 64 scopeid 0x4
	inet 192.168.1.65 netmask 0xffffff00 broadcast 192.168.1.255
	nd6 options=1<PERFORMNUD>
	media: autoselect
	status: active

# linux example
eth0      Link encap:Ethernet  HWaddr aa:00:dd:81:9d:6d
          inet addr:103.16.180.188  Bcast:103.16.180.191  Mask:255.255.255.192
          inet6 addr: fe80::a800:ddff:fe81:9d6d/64 Scope:Link
          UP BROADCAST RUNNING MULTICAST  MTU:1500  Metric:1
          RX packets:81440920 errors:0 dropped:0 overruns:0 frame:0
          TX packets:62118998 errors:0 dropped:0 overruns:0 carrier:0
          collisions:0 txqueuelen:1000
          RX bytes:17092018460 (17.0 GB)  TX bytes:137042135731 (137.0 GB)

lo        Link encap:Local Loopback
          inet addr:127.0.0.1  Mask:255.0.0.0
          inet6 addr: ::1/128 Scope:Host
          UP LOOPBACK RUNNING  MTU:65536  Metric:1
          RX packets:5449183 errors:0 dropped:0 overruns:0 frame:0
          TX packets:5449183 errors:0 dropped:0 overruns:0 carrier:0
          collisions:0 txqueuelen:0
          RX bytes:2725478097 (2.7 GB)  TX bytes:2725478097 (2.7 GB)
```

## Tools

* ping6 is the IPv6 variant of ping

## Autodiscovery

* Neighbour discovery protocol
    * uses ICMPv6 messages
* types of messages
    * RA Router advertisiment
    * NA Neighbour advertisment
    * NS Neighbour solicitation

ND RDNSS
    some sort of network discovery thing

> Operating systems that do not support either DHCPv6 or ND RDNSS cannot automatically configure name servers in an IPv6-only environment

## DHCPv6

There is a corresponding DHCPv6

## ICMPv6

There is a corresponding ICMPv6
