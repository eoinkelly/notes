# Proxies

## Configure proxy on OS X

To configure proxy locally

    1. Settings > Network > Advanced > Proxies
    2. tick "automatic proxy discovery" or manually setup proxy addresses
        * automatic proxy discover seems to just setup the available proxies
          manually - it acutally ticks the boxes for you once you hit 'Apply'

    QUESTION: how does automatic proxy discovery work on mac???
    QUESTION: firefox has both auto proxy discover and an "auto proxy discovery URL" setting ???

There are proxies for many protocols

* HTTP
* HTTPS
* SOCKS
* FTP
* RTSP (Streaming proxy)

All are configured with a hostname:port (and optional username & password)

1Password Mini listens for websocket connections on the above ports. Somehow
1password is able to ask the OS to exempt them from proxying.

```
127.0.0.1:10191 # 1password
127.0.0.1:14821 # 1password
127.0.0.1:24861 # 1password
127.0.0.1:25007 # 1password
127.0.0.1:38151 # 1password
127.0.0.1:46360 # 1password
127.0.0.1:49801 # 1password
127.0.0.1:55730 # 1password
127.0.0.1:59483 # 1password
127.0.0.1:6258  # 1password
127.0.0.1:6263  # 1password
```

## Proxy software

* squid
* nginx with HttpProxyModule
* charles
* mitmproxy
* burp suite

## Automatic proxy discovery

Discovery of proxies can happen through

* Automatic proxy detection
* DHCP
* Active Directory group policy

    TODO: find out more

## Explicit HTTP proxy

Sources

* Really good: https://mitmproxy.org/doc/howmitmproxy.html
* HTTP [RFC 2616]()
* [RFC 3143]()

Example flow

1. The proxy receives a "proxy GET" from the client.
    ```
    GET http://example.com/index.html HTTP/1.1
    ```
1. proxy optionally adds headers to the outbound request e.g.
    * `X-Forwarded-For`
1. proxy parses the GET and makes a connection to the server
1. proxy passes data from server back to client

From server POV the proxy is the "client"

* A "proxy GET" includes extra stuff
    * protocol of URL
    * domain of URL
    * maybe extra headers just for the proxy
* An explicit HTTP proxy operates at the HTTP layer can only proxy HTTP
  requests (not HTTPS or any other protocol)

```
# Send HTTP request to a local explicit http proxy

GET http://evertpot.com/http-11-updated/ HTTP/1.1
Host: evertpot.com
Proxy-Connection: keep-alive
Cache-Control: max-age=0
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8
Upgrade-Insecure-Requests: 1
User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/45.0.2454.93 Safari/537.36
DNT: 1
Accept-Encoding: gzip, deflate, sdch
Accept-Language: en-US,en;q=0.8
If-Modified-Since: Tue, 11 Aug 2015 14:26:44 GMT
```

## Explicit HTTPS

* very different to explicit HTTP

1. client makes TCP connection to proxy
1. Instead of a "proxy GET" the client sends
    ```
    CONNECT example.com:443 HTTP/1.1
    ```
1. proxy makes a TCP connection to the url specified
1. it blindly passes traffic back and forth. That traffic includes the SSL
   negotiation (connection upgrades from HTTP to HTTPS) but the proxy cannot
   see any of it.

# MITM HTTPS proxy

* MITM HTTPS proxies include a CA authoritiy implementation
* The fake CA generates fake certificates for websites on the fly.
    * These certs are generated and issued for each site you visit
    * If you connect to an IP address then mitmproxy will pause the
      conversation and connect to that IP itself and sniff the certificate
      domain so that it can fake it properly.
    * It will extract bits of the real upstream cert to use for its fake cert
      to smooth over any problems the client may have
* The client will believe these certs if the public key of the "fake CA" has been trusted.
* The server thinks the mitm proxy _is_ the client.
* In this process the client knows that it is talking to a proxy - it sends the
  `CONNCET` message as normal - but it thinks that the proxy is just passing
  encrypted messages back and forth - it does _not_ know that the proxy is
  impersonating the server.

## Transparent/intercepting HTTP(S) proxies

* transparent HTTP/S proxy = normal proxy + a routing component
* requires something to intercept the TCP connection and route it through the proxy
    * a router or firewall can do this e.g. iptables, pf
* routing mechanism passes the original destination server along to the proxy so the
  proxy can initiate a connection to that server
* ++ no proxy configuration required
* -- snooping
* implemented by ISPs for caching etc.
* performs some of the same functions as a gateway/router
* -- causes problems for HTTP caches as "some requests and responses become
  uncacheable" ???

Example flow

1. client makes connection to router or gateway (which may be a separate box,
   separate process or part of same proxy process on same box)
1. router forwards the connection to the proxy
1. proxy does its thing
1. router forwards reply back to the client

In the above flow the client does not know that there is a proxy in the chain.

### curl and proxies

* you have to explicitly tell curl to use a proxy
* curl does not use the autoconfigured proxy unless you pass the `-x` option or
  set the proxy environment variables
* Uses different environment variables for HTTP and HTTPS

```
export http_proxy=http://localhost:8888
export https_proxy=https://localhost:8888

curl -v http://foo.com --socks5 localhost:8889
```


## SOCKS proxy

* There is nothing inheriently transparent about SOCKS!
* Socket Secure (SOCKS) is an Internet protocol
* routes network packets between a client and server through a proxy server.
* SOCKS5 additionally provides authentication so only authorized users may
  access a server.
* a SOCKS server proxies TCP connections to an arbitrary IP address, and
  provides a means for UDP packets to be forwarded.
* Some SSH suites, such as OpenSSH, support dynamic port forwarding that allows
  the user to create a local SOCKS proxy.
* The Tor onion proxy software presents a SOCKS interface to its clients.
* uses a handshake protocol to inform the proxy software about the connection
  that the client is trying to make, and then acts as transparently as possible.
* Chrome
    * The --proxy-server="socks5://myproxy:8080" flag tells Chrome to send all
      http:// and https:// URL requests through the SOCKS proxy server
      "myproxy:8080", using version 5 of the SOCKS protocol.
* Aside: while HTTP proxying has a different usage model in mind, the CONNECT
  method allows for forwarding TCP connections
* Charles runs socks proxy on localhost:8889
* Operates at TCP layer (layer 5 of OSI model) i.e. proxies _TCP connections_
  and _UDP packets_.
* SOCKS Version 5
    * adds
        * additional authentication options
        * UDP support (so socks can be used for DNS lookups)
        * IPv6 support
    * [Socks 5 protocol RFC 1928](https://tools.ietf.org/html/rfc1928)
    * Socks 5 auth flow
        1. client sends a packet with a byte code representing each auth method
           it supports.
        1. The actual auth happens over the next packets (what actually gets
           sent depends on auth method)
    * examples
        * [RFC 1929 Socks 5 username & password
          auth](https://tools.ietf.org/html/rfc1929)
* Because the client must first make a connection to the SOCKS server and tell
  it the host it wants to connect to, the client must be "SOCKS enabled."
* socks is a transparent proxying mechanism
* it can have better performance than HTTP proxies
    * the client (browser) has a maximum no. of HTTP connections it will make
      to a server
    * since all client traffic goes through the proxy server that limit changes
      how many connections the client will open to the proxy.
    * this ruins any domain sharding done by the server
    * with socks the client is unaware of the proxy so this is not a constraint

`curl` does not use the socks proxy unless you explicitly tell it to so it
seems like the socks proxy on OS X is opt-in for apps i.e. it is not
transparent.

## Burp suite

* Burp functions as an HTTP proxy server
* runs on 127.0.0.1:8080 by default

## Steps to setup charles to MITM a mobile device for dev

https://www.charlesproxy.com/documentation/faqs/using-charles-from-an-iphone/

1. Open charles (it just has to be open, it does not have to be proxying maxOS traffic)
1. Get IP address of computer running Charles `ifconfig en0`
1. Go to the Settings app, tap Wi-Fi, find the network you are connected to and
   then tap the blue disclosure arrow to configure the network. Scroll down to
   the HTTP Proxy setting, tap Manual. Enter the IP address of your computer
   running Charles in the Server field, and the port Charles is running on in
   the Port field (usually 8888). Leave Authentication set to Off.
1. Accept the pop-up from charles telling you that your phone has just tried to use it.

