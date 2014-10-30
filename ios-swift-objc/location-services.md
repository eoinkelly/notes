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
