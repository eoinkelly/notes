# Node streams Handbook

- https://github.com/substack/stream-handbook

_these notes are incomplete - I will return to this when I have done more node_

- streams2 landed in 0.10
- Streams are an event based API for handling data
- they are a move away from domain specific events and methods to streams
- there are lots of things in JS that can async provide data
- rather than them all having their own way, we use streams which makes interop
  easier.
- A streams is basically a unix pipe
    - can be readable, writable or both
- Steams emit events

- A readable stream
    - will emit a `data` event whenever it has a new chunk and an `end` event
      when it is complete.
    - has `pause()` and `resume()` methods you can call
        - `pause()` is advisory - there might be a chunk in-flight so you might
          still get data after a pause

- A writable stream
    - implements `write()` and `end()` Q: do writables emit events?
    - `write()` will return true if it wrote or false if it couldn't
        - you can watch for a false return which says that the writer is blocked
          at the moment so you should back off. This creates a sort of _back
          pressure_
        - If I am implemneting `write()` returning false is advisory - my user
          my still call write() even after I return false

Steams can be useful in browser too.

Streams are a useful abstraction whenever there is an IO bound API. The browser
has lots of these e.g. XHR, WebSockets, IndexedDB, WebWorkers, WebRTC, and DOM
Events

## Steam events and their meanings

- `end` means no more data will be emitted but if this stream is also writable
  it should stay open for more writes if they need to happen
- `close` means whatever this thing was tied to, it's done now. you may dispose
  of it, it's gone.

- Node uses sterams for all its _I/O bound_ APIs e.g. TCP, file system
- Events implement the observer pattern

# Destroying a stream

To destroy a stream

```js
myStream.destroySoon(); // wait for current data to finish buffering then kill it
myStream.end(); // wait for current data to finish buffering then kill it
myStream.destroy(); // kill it forcibly
// QUESTION: what does "kill" mean here - is the object dealloc?
```

## pipe(writableStream)

- a method on readable streams that, given a writable stream, will take care of
  pumping that readable stream into the writable.
    - it handles managing a slow writer by issuing `pause` events on itself to
      not overwhelm the writer
    - when it gets its own `end` event it will call `end()` on the
      writableStream
- given a writable stream as an arg it will listen for its own `data` events and
  write them to the writable stream.
- pipe handles backpressure for you for free (provided both things are good
- streaming citizens) - because it can issue the `pause` event on the read
  stream, it will automatically not buffer too much of the contents of that
  stream into memory. QUESTION: how well does this work with TCP? Can you really
  communicate the buffering in eough granularity?

Steams mean you don't have to think about how to get data in/out of any API that
supports it. I guess they work best when everything is a stream

# copying files

Node has no built-in way to do this - they want you to use streams

What gets really passed around in streams? I believe it is either a string or a
node buffer - TODO find out more

```js
var readable = new Stream
var writable = new Stream ...

readable.pipe(writable)
```

There are 5 kinds of streams

1. readable
2. writable
3. transform
4. duplex
5. classic
    - steams from node 0.4 days

`src.pipe(dst)` returns `dst` so you can chain them together

```
a.pipe(b)
b.pipe(c)
c.pipe(d)

// same as

a.pipe(b).pipe(c).pipe(d)

// create duplex stream
a.pipe(b).pipe(a)
```

STDIN and STDOUT are streams in node land. They are represented by

- `process.stdin`
- `process.stdout`
