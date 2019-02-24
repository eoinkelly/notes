# Websockets

## Debugging websocket connections

Tools in descending order of usefulness:

1. Chrome dev tools
    * ++ lets you see headers AND messages from server
    * ++ can send messages to server in console if you can get hold of the WebSocket object
2. `wscat`
    * -- cannot see any handshake headers
    * ++ can see messages AND send messages
    * ++ can enable slash commands to interactively send control frames
3. curl
    * ++ can see handshake headers
    * ++ can see messages from server
    * -- cannot send messages

### wscat example

```
npm i -g wscat
wscat -o "http://localhost:3000" -c "ws://localhost:3000/cable"
```

### curl example

Debugging a websocket with curl:

```
curl
 -i
 -N
 -H "Connection: Upgrade"
 -H "Upgrade: websocket"
 -H "Host: localhost:3000"
 -H "Origin: http://localhost:3000"
 http://localhost:3000/cable
```

* `-i` is `--include` i.e. include protocol headers in the output
* `-N` is `--no-buffer` i.e. disablings buffering ofthe output stream

