# Routing basics

- Routing is the process of taking a newly built/received IP packet (layer 3)
  and deciding what source and destintion values to use in the **layer 2**
  header
- routing decisions are made by the OS kernel networking stack

- choosing a layer 2 source
    - A particular machine (whether host or router) has multiple network
      interfaces - it must choose one as the source of the packet and put the
      layer 2 address of that interface in the source field of the layer 2
      header
    - This process is similar whether the packet was just built on this machine
      or it was received from another machine
    - Not all network interfaces are linked to hardware - some are (wifi,
      ethernet cable) but many are virtual network interfaces created by the
      kernel for other reasons (communication with VMs, docker containers,
      localhost traffic etc.)
- choosing a layer 2 destination
    - By inspecting the layer 3 IP header for the layer-3 destination and then
      consulting tables/algorithm to decide what the best destination layer-2
      address to use that will move the packet closer to its end destination.

QUESTION: do hosts engage in any routing protocols?

QUESTION: is it as simple as searching through the routing table for a match?
how does that search work? does it prefer more specific matches?

QUESTION: where does ARP fit in to the routing algorithm? A: once the
router/host decides what interface The host uses its route table to decide what
layer-2 destination address to put on the packet sometimes the route table will
have a MAC address to dus directly - if it doesn't then then the table will
yield the layer-3 address of a gateway to us - if the layer2 address of this
gateway isn't known then ARP must be used to find it. At some point along the
way the machine must decide whether the destination layer-3 address is directly
accessible or whether it needs to use a gateway. if the address is direclty
accessible and it doesn't ahve the layer-2 address already it must use ARP to
find it and then send the packet

This is still not clear.

## Routing protocols

1. RIP
1. OSPF
1. BGP
1. others ???

Routes in osquery are one of

- gateway
- static
- router
