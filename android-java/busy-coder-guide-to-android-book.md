

4 kinds of components

1. Activity
    * like window in a desktop app
    * it usually fills the screen between the top bar and hardware buttons at bottom
    * is short lived
2. Service
    * long lived
    * can run independent of actvity that started them
    * used for scheduled tasks
3. Content provider
    * an abstraction on top of data that is shared between apps
4. Broadcast receiver
    * System and apps send out "broadcasts"
    * e.g. low battery, screen turned off etc.
    * you can register receivers of these broadcasts


Rendering UI

* Widget framework (akin to UIKit)
    * a "widget" is a "micro unit" of UI e.g. button, text box etc.
    * widgets are organised by "Layout manager" classes
    * the "layout resource file" is XML that maps layout-managers to widgets
    * "fragments" are reusable (across diff screen sizes) chunks of UI
* 2D Canvas
* 3D OpenGL


Resources
    * images, strings, UI layouts
    * live in `res/`
    * Grouped into "resource sets" by screen size

Package name
* Every app has a "package name"
* Jobs
    * used as a target to generate some source code
        * => It must be a "valid Java package name"
    * used as unique ID on the app store
* usually follow the reverse DNS format as DNS names are already forced to be unique


Dalvik VM

* The dalvik VM has differnt bytecodes than the JVM
* so you can't just run regular .jar files on the device
```
.java --> [javac] --> .class(jvm bytecode) --> [dexer] --> .dvk(dalvik bytecode) --> [zip] --> .apk
```

Threading

* android sets up a bunch of threads for the app
* most action happens on the main thread (or "UI thread")


Java versions

* supports Java 6 or Java 7
* Java7 is required for "some features"

* if you use Java 8 you need to get IDE to emit Java7 compatible bytecode


SDK versions

* MinimumSdk = the minimum version that your code will run on
* targetSdk = the version you are targetting (usually most recent release
* compileSdkVersion = the version of android you compile against
    QUESTION: how does this work?
    * new enough to give you the features you want
    * you may have to route around those features in code to support bakc to the minimumSdk


UP TO: Start of Getting around android studio

Apparently there are plans to roll more of the android tools (AVD, sdk manager etc.) into Android Studio itself

Tools

* Android Device Monitor tool
* contains 5 "perspectives"
    * DDMS (Dalvik device monitor service)
        * available as a "perspective" in Eclipse
        * Contains Logcat
    * Heirarchy view
        * available as a "perspective" in Eclipse
        * available under Android Device Monitor tool now
    * Debug
    * Pixel perferct
    * Tracer for OpenGL ES
    * Resource



R.java
    * used to control the user interface
    * just defines a bunch of constants

res/ dir
* static files packaged either directly or run through a pre-processor
* `res/raw` is the place to put "general purpose" files

.iml files hold metadata about the project


src/{sourceset}
Under `src/` are a number of source sets. By default

1. main (app code)
2. androidTest (app tests)

A source set can contain

* Java code
* Resources
    * Resources are "compiled" into a `resources.arsc` file in your apk (except `res/raw/`)
* Assets (static files to be packages in the apk)
    * Assets are just copied into the apk unmodified
* AndroidManifest.xml


Some of the things setup in AndroidManifest.xml can be _overridden_ in gradle!






# Misc jumping around for the pack
Location based access

need perms in the manifest

ACCESS_COURSE_LOCATION
ACCESS_FINE_LOCATION



You can ask for locations

ways of calling requestLocationUpdates()

1. Supply a `Criteria` object and ask for locations that best fit the criteria (from any provider)
2. ask for locations providing a `Looper` as a parameter so updates are given to a background `HandlerThread` instead of the main UI thread
3. ask for locations providing a `PendingIntent` which will be executed instead of calling the `LocationListener`

Google play services offers the "fused" location provider which simplifies getting locations


PendingIntent

A description of an Intent and target action to perform with it
can be handed to other applications so that they can perform the action you described on your behalf at a later time.


# Services

* A single instance of the service lives in memory
* multiple calls to startService() will not start more than one


To create a service (similar to how you create an activity)

1. Inherit from the `Service` base class or one of its children
    * Service (an abstract class)
    * IntentService (< Service)
2. Override some lifecycle hooks
    * onCreate()
    * onStartCommand()
    * onBind()
        * abstract method so always needs to be implemented even if not using it
    * onDestroy()
        * NOTE: might not get called if device runs out of RAM
3. hook it into the system via the manifest
    * use `<service android:name="SomeService">`
    * since the service is in the same namespace as the rest of the app you can skip the full namespace name if you want


### 2 ways of sending data to a service

1. send a command
    * no lasting connection
2. bind to the service
    * establish a comms channel that lasts as long as the client needs it
    * -- more setup required
    * -- harder to manage across configuration changes
        * i assume if app is upgraded that the service may not be restarted
    * ++ full rich API between client and service - makes the service look like a local object

* startService()
    * you pass an Intent which has 2 jobs
        1. tell the system which service you want to communicate with
        2. pass params to the service via Intent extras
    * is async so will not block
    * will start the service if it is not already running
    * the Intent it gets will be given to `onStartCommand()` in the service


* onStartCommand()
    * is run on main thread so should do its work and finish quickly
    * return value indicates what should happen if service is killed
        * `START_STICKY` - system should re-start the service but not re-deliver the Intent
        * `START_REDELIVER_INTENT` - restart the service and deliver the Intent again
        * `START_NOT_STICKY` - remain stopped until explicitly started by app code

### 5 ways of commiunicating _from_ services

1. Broadcast intents
2. Pending results
3. Event buses
    e.g. `LocalBroadcastManager`
4. Messenger
5. Notification


# Intents

* An Intent provides a facility for performing late runtime binding between the
  code in different applications.
* Its most significant use is in the launching of activities, where it can be
  thought of as the glue between activities.
* It is basically a passive data structure holding an abstract description of an action to be performed.
* Roughly analagous to an expanded version of a HTTP request

Anatomy of an intent:

* primary attributes
    * action e.g.
        * `ACTION_VIEW`
        * `ACTION_EDIT`
        * `ACTION_MAIN`
        * `ACTION_DIAL`
        * `ACTION_GET_CONTENT`
        * expressed as a constant in Java so you might need to use `path.to.the.ACTION_THING` to get at it
    * data Uri
        * the data to operate on, expressed as a Uri
            * could be a URI that uniquely identifies a row in a local database of some kind
            * e.g.
                * content://contacts/people/1
                * tel:123
* secondary attributes
    * categories
        * CATEGORY_LAUNCHER
            * this component shows up on the launcher screen
        * CATEGORY_DEFAULT
        * CATEGORY_ALTERNATIVE
            * show in list of alternative actions a user can perform on a piece of data
    * type
        * the mime type that this intent can handle
    * component
        * the class of the object that is supposed to receive this Intent
        * an "explicit intent" is addressed to a single component
        * an "implicit intent" this is blank and the component to run is found by inspecting the other fields
    * extras
        * a `Bundle` of extra data info you want to pass along

Intents are handled by one or more of

* Activity,
* BroadcastReceiver, or
* Service


An "intent filter" can be expressed in Java via the `IntentFilter` class or in XML via `<intent-filter>`

The `PackageManager` is the thing that is queried to find a component that will handle a given implicit event - it answers based on the (action, type, categories) of the Intent






There are standard actions, categories, extras defined in Android

Two main ways of addressing Intents

1. If you are sending the intent to somehting within your app you can name the
   component you want to receive it.
2. If you are sending the intent outside your app then you should use the
   action+data-uri+mime-type combo to describe the Intent and let the system
   decide what should handle it

How an activity expresses which intents it wants

* Any component that wants to be started by an intent puts and
  `<intent-filter>` block in the manifiest. A "component" has an
  "intent-filter" where it declares what kinds of intents it should be told
  about
* You do not need an intent-filter for Intents sent directly to a named component

```xml
<!--
    declare that this is the "main" activity for the activity that encloses it
    declare that it is in the "LAUNCHER" category which means its icon should be put in the launcher
-->
<intent-filter>
    <action android:name="android.intent.action.MAIN" />
    <category android:name="android.intent.category.LAUNCHER" />
</intent-filter>
```

```java
Intent foo = new Intent(activityInstance, SomeService.class);


// foo is a broadcast targetted at the SomeService component
// foo must be registered in the manifest
sendBroadcast(foo);
```

* can have "extras" attached - each extra has a type
* the extras make a dictionary - you lookup by key

intent.getStringExtra('KEY')
intente.getBooleanExtra('KEY')
it seems you can suply a default value too
latitude = intent.getDoubleExtra("latitude", -1);


can be targetted at a "component" or at an "action"

# Context

context is a reference to linking your resources to your program. Each object
is given its own context, which contains the resources required to set that
object up.

The context for an item can come from a variety of places. Sometimes it is
stored and has to be retrieved, sometimes it is inherited.

Within an Activity
    * Activity inherits from Context so you can just pass `this` as a Context instance
    * `getBaseContext()` from within an activity will get you ??? (broader application context
Within a View
    * does not inherit from Context
    * `getContext()` will get you the (not very complete) View context
Within a Fragment
    * does not inherit from Context
    * `getActivity()` will get you the Activity which is the context for the fragment (Activity extends Context
Within a BroadcastReceiver
    * they don't "have" a context
    * they get a particular context when an Intent is received - they may get a different context the next time!
        * `onReceive(Context c, Intent i)`


```
// class heirarchy relevant to Context
java.lang.Object > android.content.Context > android.content.ContextWrapper > android.view.ContextThemeWrapper > android.app.Activity
java.lang.Object > android.content.Context > android.content.ContextWrapper > android.app.Service
java.lang.Object > android.content.Context > android.content.ContextWrapper > android.app.Application
// notice that Application, Service, Activity are all "Context" objects
```

Application

* `Application inherits from `Context`
* created when the app starts up
* there is only one instance of it in the program
* We can subclass `Application` and use that instead (wire it up via `android:name` attr of `<application>` element in manifest
* Get the `Application` instance by calling `getApplicationContext()` on any `Context` object
* Uses
    * If we an Activity|Service and are holding Thing in a static data member
      and Thing needs a context then we should not use ourselves because when
      we get GC the static data member's context will still refer to us even
      though we are gone. For this reason we want our static data member to use
      a context that we know is always available i.e. `Application` instance.
* so if I am storing an Intent in a Service as a _static_ data member then I should definitely use `getApplicationContext()` as its context becasue my `Activity` instance might go away and the static member will still be around
