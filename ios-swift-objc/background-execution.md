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

# background locations

Tracking location in the background is a special case. The options are:

1. significant change location service
    * does not provide "high precision" location data
    * is the most battery friendly way to do location tracking
    * the system will wake the app whenever the users location changes "significantly"
    * if the app is terminated after starting this service the system will restart it to get the update
2. foreground only location services
    * uses standard Core Location service to get location data
    * stops delivering updates if the app is suspended
3. background location services
    * uses standard Core Location service to get location data
    * requires that the system monitor location via radio so is battery hungry
    * enabled by setting UIBackgroundModes to 'location' in Info.plist
    * does not prevent the system from suspending the app (unlike 'audio' which
      does prevent your app from entering the "suspended" state).
    * does tell the system that it should wake the app whenever there is a new location to deliver
        * if the user force quits you this will not happen
