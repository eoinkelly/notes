# Server pushed data

key questions:

1. will my clients be
    * totally consumer
    * mostly consumer
    * equal consumer and producer
    * mostly producer
    * totally producer
    of messages?
1. How "realtime" does it need to be? All of these transports are capable of
   "soft realtime" but some have potentially better latency than others.

    QUESTIONS
    is 500 connections == 500 concurrent users in websockets?

Good articles:

* http://blog.fanout.io/2013/03/04/long-polling-doesnt-totally-suck/


* Elixir phoenix implements channels using "transport adapters". Transports are
  websockets (default) and HTTP long poling (fallback)


All of these technologies require a client and a server part. Bothe client and server

* implements "transports" (e.g. websockets, HTTP long poling)
* provides an abstraction on top of them of channels and messages

* server that implements the given transport protocols and provides an
  abstraction on top e.g.
    * elixir phoenix
    * ruby messagebus

## The abstraction

messages and channels

## The transport options

1. HTTP poling
    * can be implemented in JS with `setInterval()`
    * implementations
        * https://github.com/SamSaffron/message_bus (will fall back to it if
          long poling not available)
    * ++ supported in *every* browser that supports JS
    * client makes a new request for each "pole"
    * -- half-duplex, uni-directional
2. HTTP long poling
    * also called "hanging GET"
    * also called COMET ???
    * flow:
        1. client makes request
        2. server does not have data yet but holds a connection open for TIMEOUT
        3. when server has data it sends to client and closes connection
        4. client immediately makes a new connection
    * -- ?
    * ++ no special browser support required
    * ++ Long-polling simplifies on the client side by rolling together the
         notification mechanism and the backstore query/ping into a single request
    * implementations
        * https://github.com/SamSaffron/message_bus
3. HTTP streaming
    * server keeps connection open indefinitely - is a bit like long poling
      except it does not close connection after sending data
    * TODO: https://tools.ietf.org/html/rfc6202
4. Web sockets
    * uses the `ws://` and `wss://` protocols
    * Both schemes use an HTTP upgrade mechanism to upgrade to the Web Socket protocol
    * QUESTION: are WS more reliable over https because proxies and firewalls fuck it up less?
    * > WebSockets were always intended for only one specific thing—allowing
      > web browsers and web servers to speak connection-oriented, stateful wire
      > protocols (like IRC or IMAP) at one-another over an HTTP tunnel.
    * can be implemented in flash in browser if browser does not support natively
    * Browser support for WS is pretty good. Everything except IE9 and older and
    Android 4.3 and older
    * ++ bi-directional full-duplex protocol
    * ++ good if the client will send a lot of data too
    * Commercial websocket as a service thing: https://pusher.com/
        * paid service
        * pricey
        * unlimited channels on all plans
        * 500 max connections = $50/month
        * focuses on websockets but will fall back to
            * flash websocket client
            * HTTP (they don't say whether they do streaming/long poling)
    * implementations
        * https://github.com/imanel/websocket-ruby
            * just websockets
            * not channels and messages abstraction
            * seems popular
        * EM-Websocket
            * used by Pusher
            * used by Livereload
        * action cable
            * uses redis pub/sub to implement websocket server
            * runs a separate server to your rails app
            * not baked yet
5. Server sent events
    * ++ sent over HTTP so proxies and firewalls are happy
    * ++ automatically reconnects after ~3s if connection fails - WS do not do this
    * ++ sent over HTTP so no special server required
    * ++ can send "arbitrary events" ???
    * ++ events have ids which means the browser can keep track of which events
         have had their handlers run in cases where the connection is dropped
    * ++ pretty simple implementation in browser and on server
    * message has `Content-Type: text/event-stream`
    * message bodies are plain text.
    ```
    id: 12345\n
    data: Stuff\n
    data: and nonsense\n\n
    ```
    * real world experience notes from abletech talk
        * uses vanilla http - nice for proxies etc.
        * good browser support except no IE at all http://caniuse.com/#feat=eventsource
        * there is a polyfill
        * a w3c standard
        * implemented in server in a rails controller that follows the spec
        * you need to config your load-balancer/web server
            * disable caching
            * disabler buffering
        * they use a heartbeat event to keep the connection alive (they go every 60sec)
            * it keeps alive the connection
            * it does tie up a connection in the browser
        * the response SSE should be really small - it just triggers to the JS in the browser that it should make a new request
        * a long lived http connection initiated by browser
        * mobile browser support ?
        * can cause problems for passenger as responding to SSE ties up a process - your ruby process will block on IO - watch my gear use puma+MRI
            * MRI will run other threads while waiting on IO
            * you need a big thread pool.
        * they use redis as the queue for the events
            * you need a unique redis connection for each thread!
        * vs. web sockets
            * websockets have firewall problems
    * the don't seem to be supported as fallback by the things that support websockets

Points to compare on

1. how much data is transfered
1. latency of messages
1. how does the client catch-up on missed messages
1. how is presence managed
1. what ways does it load server
    * keeping connections open
    * memory usage
    * CPU usage
1. how does is scale if you have to have more than one server?
1. how HTTP/2 will affect it
1. how TLS works with it
1. amount of code required on server to implement
1. amount of code required on client to implement
1. problems with proxies and firewalls

## Proxies and firewall support

Some network devices do not pass websocket traffic properly or at all e.g.
proxies and firewalls

* Gem implementing Bayeaux protocol: http://faye.jcoglan.com/


## Impact of HTTP/2

* There is some overlap but websockets and hTTP/2 have different goals

http://stackoverflow.com/questions/28582935/does-http-2-make-websockets-obsolete

https://bloggeek.me/websockets-http2/
