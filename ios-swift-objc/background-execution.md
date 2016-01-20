# Background execution

This is different to doing work concurrently

Most apps:

    foreground (active) state -> background state (short time) -> suspended state

There are 3 categories of "background support" provided by iOS

1. app starts a task in the foreground state and needs time to finish it after
   being backgrounded.
2. App starts downloads in foreground and then hands of management of them to
   the system so that if the app is suspended or terminated then the download
   continues
3. App needs to run in the background to support specific kinds of tasks can
   _declare_ their support for one or more _background execution modes_
    * The system maps the modes you declare to sets of events that it will wake
      your app in response to.


# code sample


```objc
__block id tself = self;

//
dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
    [tself startLocationUpdates];

    // get the "runloop object" for the current thread and then call run on it
    [[NSRunLoop currentRunLoop] run];
});
```

dispatch_async(queue, block)
* puts the given block on the given queue
* returns immediately
* part of GCD

dispatch_get_global_queue(identifier, flags)

* returns a global concurrent queue that executes at the given quality-of-service class

