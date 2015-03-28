# server sent events notes from abletech talk

* uses vanilla http - nice for proxies etc.
* needs browser support
* there is a polyfill
* w3c standard
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
