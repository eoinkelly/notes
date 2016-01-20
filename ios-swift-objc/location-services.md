# Location services

Steps to setup a project
1. add 'location-services' and 'gps' (if you need the accuracy) to the
   UIRequiredDeviceCapabilities in Info.plist
2. link to the _Core Location_ framework


Reasons location services might not be available:

* Device in flight mode
* User denied access
* User disabled location services in settings

You can check their availability with

```objc
[CLLocationManager locationServicesEnabled] // returns YES if they are, otherwise prompts user
```

There are 2 types of location services available:

1. Standard
2. Significant change
    * 500 M or more



* Significatn change location service
    * will wake up the app after it has been "suspended" or "terminated"
        * when it is woken up you have to re-start location services and send your update within 10sec
    * if the user disabled "background app refresh" either globally for for your app then this wake-up will not happen
        * on phone `Settings > General > Background app refresh`



currently pack not listed in apps taht require BAR


# Terminology

"suspended"
"terminated"

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
