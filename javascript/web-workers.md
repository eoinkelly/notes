- http://www.html5rocks.com/en/tutorials/workers/basics/
- https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Using_web_workers

- you can also set listen to the `onerror` event from the worker - any
  exceptions it throws will go there

```js
// main.js
var worker = new Worker('-worker.js');

// evt is an instance of {MessageEvent}
// evt.data is the content that was passed to postMessage
// evt.currentTarget seems to be a reference to the worker
worker.addEventListener('message', function (evt) {
    console.log('reply from worker', evt, evt.data);
    console.log(
        'is currentWorker a ref to worker: ',
        worker === evt.currentTarget
    );
});

worker.postMessage('do stuff please'); // start the worker
// worker.terminate(); // kill worker
```

```js
// worker.js
// self is the reference to the worker within the worker
//
// this is alos a reference to the worker

// evt is an instance of {MessageEvent}
// evt.data is the content that was passed to postMessage
// evt.currentTarget is ???
self.addEventListener('message', function (evt) {
    console.log('e in worker', evt);
    self.postMessage('hi from worker');
    self.close(); // terminate myself
});

// IMPORTANT: Messages passed between the main page and workers are copied, not shared.
```
