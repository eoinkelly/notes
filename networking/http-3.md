# QUIC

Tips

- Enable the "protocol" column in browser dev tools to see which protocol in use

Overview

- A modern replacement for TCP for web traffic
    - built on top of UDP
    - IP > UDP > QUIC > HTTP/3
- RFC is official as of June 2021
    - RFC 9000 is main one, supported by RFC 8999 -> 9002
    - https://www.rfc-editor.org/rfc/rfc9000.html
- Implementations
    - https://github.com/cloudflare/quiche seems to be used all over the place
      (Rust, by Cloudflare)
    - as of Jun 2021 curl needs a custom build to use it
- a bit like "TCP + TLS" layer, built on top of UDP layer
- provides reliable transport (data retransimission supported, data arrives in
  order)
- provides **independent** streams i.e. if a stream loses a packet then it
  doesn't block the other streams
- there are plans to put other protocols on top of QUIC e.g. DNS, WebRTC
- Gotcha: "Google QUIC" was an experimental early version which bundled both the
  transport layer (now IETF QUIC) and the HTTP/3 bit
- Includes TLS 1.3+
    - this means that a quic library has to include a TLS lib which can
      complicate adoption

# HTTP/3

https://http3-explained.haxx.se/en

- Challenges to changing Internet protocols
    - middle boxes in the Internet (routers, gateways etc.) are very rarely
      upgraded so new layers need to go over old layers. You cannot introduce a
      new protocol. \* This is why SCTP failed.
    - You can't even introduce a new compression protocol (e.g. brotli) safely
      unless it's within an encrypted packet that those middle boxes can't see
- Looks the same as older versions of HTTP in browser dev tools
    - The protocol is binary and multiplexed etc. but the browser can unravel it
- HTTP/3 doesn't erally add new features over HTTP/2 - it just makes those
  features better
    - Has no support for clear-text transmission (HTTP/2 technically supports
      clear-text)
    - Has independent streams (steams in HTTP/2 are dependent)
    - Header compression with QPACK (HTTP/2 used HPACK)
    - supports server push like HTTP/2
    - supports early data better than HTTP/2
    - Has 0-RTT handshake (unlike HTTP/2)
        - If you are going back to a server you already visisted you can just
          send your request
- HTTP/3 will be faster than /2 but nobody is sure how much yet
    - especially for those on bad networks
- Upgrade happens when server sends back the `Alt-Svc` header, then a supporting
  browser can open a HTTP/3 connection based on the details in that header.
    - Example:
        - `alt-svc: h3-27=":443"; ma=86400, h3-28=":443"; ma=86400, h3-29=":443"; ma=86400`
    - Note that **this is the documented way to get to HTTP/3** so in theory you
      don't need a standard port but UDP 443 seems to be the conventional port
- Support
    - (Jun 2021)
        - Cloudflare supports it
        - AWS ELB supports HTTP/2 but not 3 yet
        - mitmproxy doesn't support it yet
        - Wireshark supports it but you can't see much because it's all
          encrypted
- Problems
    - It is difficult to tell the diff between a QUIC connection attempt and a
      DDoS attempt - they both just send "random noise" as first packet
    - The UDP stacks in Linux etc. haven't had nearly the same attention as the
      TCP stack has - there will be perf improvements eventually
    - QUIC takes 2x-3x more CPU to serve a request
    - It might not be a big enough improvement to motiviate most server
      operators to upgrade to it
- Prioritization of streams has been removed from HTTP/3 but will be worked on
  in future

# History of HTTP versions

- HTTP (1996)
- HTTP/1.1 (1997)
    - IP > TCP > HTTP
    - browsers open many connections to get over how slow setting up a TCP
      connection is (slow start etc.)
        - problems
            - TCP expensive and slow to setup (slow start)
            - HTTP head of line blocking
- HTTPS (1995)
    - IP > TCP > TLS > HTTP
- HTTP/2 (2015)
    - IP > TCP > TLS > HTTP
    - moved from many connections to a single connection with a long-lived TCP
      connection
    - Still has TCP head of line blocking
        - things go bad if a packet is lost bcause it blocks all the streams
          using the connection
        - doesn't go well in a packet lossy network connection
    - You can technically do it over HTTP but many routers/gateways will break
      it so functionally it's https only
- HTTP/3 (2021)
    - IP > UDP > QUIC > HTTP/3
    - needed changes to make HTTP better over QUIC
    - is HTTP over QUIC
    - You cannot do HTTP/2 over QUIC
