# Server pushed data

TL;DR Conclusion as of 2018-12-28: MessageBus looks like the best choice for
Ruby because

- battle tested with Discourse
- has a durable queue (clients can catch up on missed messages)
- "real time" enough for most applications
- seems to have more predictable scaling behaviour than actioncable

Good background articles:

- https://samsaffron.com/archive/2015/12/29/websockets-caution-required
- https://www.speedshop.co/2015/09/30/action-cable.html
- https://blog.ably.io/rails-5-actioncable-the-good-and-bad-parts-1b56c3b31404
- http://blog.fanout.io/2013/03/04/long-polling-doesnt-totally-suck/

Some key questions to answer before choosing which technology will work for the
given problem:

1. How will each client use messages
    - totally consumer
    - mostly consumer
    - equal consumer and producer
    - mostly producer
    - totally producer
1. How "realtime" does it need to be? All of these transports are capable of
   "soft realtime" but some have potentially better latency than others.
    - Basecamp went for 10 years implementing chat with a 3 second poll
    - As of 2018-12-28 Gmail does not seem to use Websockets

## Things to watch out for

- You will likely have more successful websocket connections if you use TLS
  because proxies will get in the way less
- Browser support for websockets seems to be really solid now - basically
  everything works https://caniuse.com/#search=web%20socket
- Proxies and firewall support
    - some proxies might not support WebSocket protocol,
    - other proxies simply don’t like persistent connections and terminate them.
    - Some network devices do not pass websocket traffic properly or at all e.g.
      proxies and firewalls
    - I _think_ this means you still need a fallback for websockets (Actioncable
      doesn't have a fallback!)

Q: How much is this still a problem in 2018?

## Points to compare implementations on

1. how much data is transfered
2. latency of messages
3. how does the client catch-up on missed messages
4. how is presence managed
5. what ways does it load server
    - keeping connections open
    - memory usage
    - CPU usage
6. how does is scale if you have to have more than one server?
7. how HTTP/2 will affect it
8. how TLS works with it
9. amount of code required on server to implement
10. amount of code required on client to implement
11. problems with proxies and firewalls
12. How is authentication handled

## Options

1. Elixir Phoenix
    - Elixir phoenix implements messages & channels abstractions using
      "transport adapters"
    - Transports are websockets (default) and HTTP long poling (fallback)
2. Action cable
    - Good article:
      https://blog.ably.io/rails-5-actioncable-the-good-and-bad-parts-1b56c3b31404
    - uses redis pub/sub to implement websocket server
    - runs a separate server to your rails app (but can be mounted as rack app
      in your rails server)
3. Ruby MessageBus
    - Used by discourse at scale
    - https://github.com/SamSaffron/message_bus
4. https://github.com/imanel/websocket-ruby
    - just websockets
    - not channels and messages abstraction
    - seems popular
5. EM-Websocket
    - used by Pusher
    - used by Livereload
6. Pusher
    - Commercial websocket as a service thing: https://pusher.com/
    - paid service
    - pricey
    - unlimited channels on all plans
    - 500 max concurrent users (aka "connections") = $50 USD/month
    - focuses on websockets but will fall back to
        - HTTP streaming and HTTP polling
        - flash websocket client
7. Faye http://faye.jcoglan.com/
    - Implementes the Bayeaux protocol:
      https://docs.cometd.org/current/reference/index.html#_bayeux
    - Ruby & JS implementations
    - Comet is an umbrella term to cover all the ways of doing soft real-time
      comms that **isn't** websockets
    - Question: what's the deal with this? What are the pro/cons? [TODO]

Notes on implementations

- All of these implementations require a client and a server part (client part
  is in JS)
- Both the client and server:
    - implement "transports" (e.g. websockets, HTTP long poling)
    - provide a channels and messages abstraction on top of the transports

## The transport options

1. HTTP poling
    - can be implemented in JS with `setInterval()`
    - implementations
        - https://github.com/SamSaffron/message_bus (will fall back to it if
          long poling not available)
    - ++ supported in _every_ browser that supports JS
    - client makes a new request for each "pole"
    - -- half-duplex, uni-directional
2. HTTP long poling
    - also called "hanging GET"
    - flow:
        1. client makes request
        2. server does not have data yet but holds a connection open for TIMEOUT
        3. when server has data it sends to client and closes connection
        4. client immediately makes a new connection
    - -- Services like Heroku will kill the connection if it sits idle for more
      than 30 sec
    - ++ no special browser support required
    - ++ Long-polling simplifies on the client side by rolling together the
      notification mechanism and the backstore query/ping into a single request
3. HTTP streaming
    - server keeps connection open indefinitely - is a bit like long poling
      except it does not close connection after sending data
    - TODO: https://tools.ietf.org/html/rfc6202
4. Web sockets
    - uses the `ws://` and `wss://` protocols
    - Both schemes use an HTTP upgrade mechanism to upgrade to the Web Socket
      protocol
    - QUESTION: are WS more reliable over https because proxies and firewalls
      fuck it up less?
    - > WebSockets were always intended for only one specific thing—allowing web
      > browsers and web servers to speak connection-oriented, stateful wire
      > protocols (like IRC or IMAP) at one-another over an HTTP tunnel.
    - can be implemented in flash in browser if browser does not support
      natively
    - Browser support for WS is very good. Everything except IE9 and older and
      Android 4.3 and older
    - ++ bi-directional full-duplex protocol
    - ++ good if the client will send a lot of data too
5. Server sent events
    - -- No IE or Edge support at all
    - ++ sent over HTTP so proxies and firewalls are happy
    - ++ automatically reconnects after ~3s if connection fails - WS do not do
      this
    - ++ sent over HTTP so no special server required
    - ++ can send "arbitrary events" ???
    - ++ events have ids which means the browser can keep track of which events
      have had their handlers run in cases where the connection is dropped
    - ++ pretty simple implementation in browser and on server
    - message has `Content-Type: text/event-stream`
    - message bodies are plain text.
    ```
    id: 12345\n
    data: Stuff\n
    data: and nonsense\n\n
    ```

    - real world experience notes from abletech talk
        - uses vanilla http - nice for proxies etc.
        - good browser support except no IE at all
          http://caniuse.com/#feat=eventsource
        - there is a polyfill
        - a w3c standard
        - implemented in server in a rails controller that follows the spec
        - you need to config your load-balancer/web server
            - disable caching
            - disabler buffering
        - they use a heartbeat event to keep the connection alive (they go every
          60sec)
            - it keeps alive the connection
            - it does tie up a connection in the browser
        - the response SSE should be really small - it just triggers to the JS
          in the browser that it should make a new request
        - a long lived http connection initiated by browser
        - mobile browser support ?
        - can cause problems for passenger as responding to SSE ties up a
          process - your ruby process will block on IO - watch my gear use
          puma+MRI
            - MRI will run other threads while waiting on IO
            - you need a big thread pool.
        - they use redis as the queue for the events
            - you need a unique redis connection for each thread!
        - vs. web sockets
            - websockets have firewall problems
    - the don't seem to be supported as fallback by the things that support
      websockets

## Impact of HTTP/2

- There is some overlap but websockets and HTTP/2 have different goals
    - http://stackoverflow.com/questions/28582935/does-http-2-make-websockets-obsolete
    - https://bloggeek.me/websockets-http2/
