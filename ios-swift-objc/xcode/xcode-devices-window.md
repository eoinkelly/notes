want to see the console log on the web it needs to be in the correct order

at 16:16 we enter background at 16:33 we did our last send

# Xcode window: Devices

- "Device logs" seems to be crash dumps
    - A "ps aux" alike list of which processes were in memory when it happened

- You can save the console log to a file
    - seems to keep approx 2k lines of console log (~ 292 KB)

- Take screenshot
    - automatically put on ~/Desktop

- cog menu off the selected app
    - can view/download/replace the apps "Container"

## App "container"

TODO: what is this

# NSLog

ehind the scenes, the data passed to the NSLog function is logged using the
Apple System Log (ASL), which is Apple’s alternative for syslogd. On iOS
devices, the data logged using ASL appears to be cached until the device is
rebooted.

-weak_framework CoreFoundation -weak_framework UIKit</string> -weak_framework
AVFoundation -weak_framework CoreMedia -weak-lSystem -force_load
"$(BUILT_PRODUCTS_DIR)/libCordova.a"

App
