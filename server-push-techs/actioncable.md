# Actioncable

- is websockets **only** - it does not have a fallback!

```javascript
// Append to app/assets/javascripts/cable.js to get logging
ActionCable.startDebugging();
```

    Q: Doesn't seem to use any websocket control frames???
       Actioncable server "ping" is a 0x01 text frame with a JSON payload not a control frame. Why?

Example of an actioncable JS request to setup a websocket connection:

```
# Client handshake request
GET ws://localhost:3000/cable HTTP/1.1
Host: localhost:3000
Connection: Upgrade
Pragma: no-cache
Cache-Control: no-cache
User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/73.0.3649.0 Safari/537.36
Upgrade: websocket
Origin: http://localhost:3000
Sec-WebSocket-Version: 13
Accept-Encoding: gzip, deflate, br
Accept-Language: en-GB,en-US;q=0.9,en;q=0.8
Cookie: _chatter_session=VIjlL%2Fv4zlLTnV...
Sec-WebSocket-Key: sFxU73F9yfSIrlWCeF+BqA==
Sec-WebSocket-Extensions: permessage-deflate; client_max_window_bits
Sec-WebSocket-Protocol: actioncable-v1-json, actioncable-unsupported


# Server handshake response
HTTP/1.1 101 Switching Protocols
Upgrade: websocket
Connection: Upgrade
Sec-WebSocket-Accept: IzKxoXpcLjyJeNvioMt0yVWnDPo=
Sec-WebSocket-Protocol: actioncable-v1-json
```

Note that

- The client asked for extensions `permessage-deflate; client_max_window_bits`
  but the server include a `Sec-WebSocket-Extensions` header so no extensions
  will be used
    - `permessage-deflate`
        - has https://tools.ietf.org/html/rfc7692
        - is a registered protocol with IANA
          https://www.iana.org/assignments/websocket/websocket.xml
        - `client_max_window_bits` is part of `permessage-deflate`

- The client stated that it supported protocols
  `actioncable-v1-json, actioncable-unsupported`, the server indicated it wanted
  to use `actioncable-v1-json` so that is what the connection will use.
- Actioncable has not registered it's `actioncable-v1-json` with IANA
  https://www.iana.org/assignments/websocket/websocket.xml

## Adapters

Actioncable provides the following adapters

1. inline
    - seems to use a Hash to manage connections
2. asynchronous
    - inherits from _inline_
3. PostgreSQL
    - uses Postgres pub/sub via one of ActiveRecord connection pool
4. evented Redis
5. non-evented Redis

I have found very few docs about them
https://guides.rubyonrails.org/action_cable_overview.html#adapter-configuration
