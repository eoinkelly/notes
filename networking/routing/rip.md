# RIP

## Sources

* http://www.enterprisenetworkingplanet.com/netsp/article.php/3609151/Networking-101-Understanding-RIP-Routing.htm

## Background

* one of the oldest and simplest routing protocols
* can only travel 15 hops at most.
* designed to allow routers to exchange network information with each other
* is an example of an "interior gateway protocol"
* two versions
* RIP v1
    * sends a **broadcast** to every host on the network every 30 seconds
        * each host must process a broadcast packet to see if they care about it
    * does not support CIDR addressing (i.e. ony supports class based IP addressing)
* RIP v2
    * sends a **multicast** to every host on the network every 30 seconds
        * hosts can ignore multicast more easily (QUESTION how?)
* RIP is an application layer protocol sent over UDP
* RIP sends info every 30 seconds to UDP port 520
* Every RIP packet contains a command, a version number, and a routing domain. Then up to 25 routes will follow in the same packet.

QUESTION: what do modern wifi networks use?

## Downsides

* slow convergence
* split horizons (TODO ???)
* authentication is only plain text
* hop limit of 15 hops

```
It isn't the hop limit that makes us groan about RIP, it is the covergence time, blind distance vector, and full update advertisements every 30 seconds that cause it not to be everyones favorite.

Several production networks I have see still use in in areas, due to some system older device in that network that only speaks RIP.

The Internet uses BGP, and most organizations use OSPF.   Some ISPs use ISIS, and a few still use EIGRP.

RIP stil makes its way into CCNA testing.
```

Routing tables (also called _Routing Information Base_)

* A list of the "best" routes supported by "each" routing protocol


Aside: What does Unifi gear support?


