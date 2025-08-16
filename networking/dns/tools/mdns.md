# mdns (.local addresses)

- uses port 5353
- multicast DNS (mDNS) protocol resolves hostnames to IP addresses within small
  networks that do not include a local name server.
- It is a zero-configuration service, using essentially the same programming
  interfaces, packet formats and operating semantics as unicast Domain Name
  Service (DNS).
- It was designed to work as either a stand-alone protocol or compatibly with
  standard DNS servers

Implemented by

1. Apple Bonjour
2. Avahi (open source) https://www.avahi.org/doxygen/html/
3. a somewhat limited version built into Windows
4. systemd-resolved

Can work with DNS-SD (DNS Service Discovery) which is a companion protocol

1. Querying machine sends an IP multicast quer asking the host with that name to
   identify itself
1. Target machine then multicasts a message that includes it's IP address
1. All machines within hearing can update their mDNS caches
1. A host can relinquish it's claim on a name by sending a response with a TTL
   of 0

By default it exclusively resolves hostnames ending with `.local`

The contents of the query and response are very similar to normal DNS

## mDNSResponder (macOS)

The mDNS Responder Daemon (mDNSResponder) serves both as a DNS Stub Resolver, as
a resolver for information published using multicast DNS (mDNS), and as a
publisher of mDNS information. mDNSResponder monitors multicast traffic on port
5353, the mDNS port, to keep track of services advertised on the local network.
mDNSResponder performs DNS resolution for non-local queries, and resolves
queries in the special '.local' domain using mDNS.

    _<thing>._tcp.local seems to be a common address pattern

    e.g.

    _printer._tcp.local
    _scanner._tcp.local
    _googlecast._tcp.local
    _smb._tcp.local
    _homekit._tcp.local
    <macos-computer-name>._airplay._tcp.local

it's quite a noisy protocol.
