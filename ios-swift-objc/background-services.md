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
3. App needs to run in the background to support specific kinds of tasks  can
   _declare_ their support for one or more _background execution modes_
    * The system maps the modes you declare to sets of events that it will wake
      your app in response to

tracking location in the background is a special case. The options are:

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
      does prevent your app from entering the "suspended" state.
    * does tell the system that it should wake the app whenever there is a new location to deliver
        * if the user force quits you this will not happen


 QUESTION: do we want the whole app woken up and put back into memory each time?
    what are the alternatives? can we wake up a thread
    we cannot deallocate any heap resources










# raw shit
definitely on ios6 you had a 10 minute window in suspended state

apple limits the kind of apps that can run in the background

ios7 multitasking APIs

> Apple encourages to use NSURLSession API which creates a background session and enqueues transfers

> The new UIBackgroundModes which include "Background Fetch" and "remote-notifications" are also introduced in iOS7 which help in running background services

> You can declare app's supported background tasks in Info.plist using X Code 5+. For eg. adding UIBackgroundModes key to your app’s Info.plist file and adding a value of 'fetch' to the array allows your app to regularly download and processes small amounts of content from the network. You can do the same in the 'capabilities' tab of Application properties in XCode 5


# Example of the pack

We are executing startLocationUpdates and stopLocationUpdates as tasks on the global queue (not the main queue) and then running
        [[NSRunLoop currentRunLoop] run];

I suspect we are not running things on a background queue?
    does that matter?


[NSRunLoop currentRunLoop] // access the runloop for the current thread

our current implementation seems to use timers and a runloop manually
