
    TODO: How does cordova run xcode from cmd line?


# Cordova plugin for iOS

## The role of config.xml

```xml
<feature name="serviceName">
  <param name="ios-package" value="EKObjCClassName" />
  <param name="onload" value="true" />
</feature>
```

* The name attribute of a feature _must_ match the serviceName you use in the `cordova.exec` call.
* Add any hosts your plugin needs to the domain whitelist too


## Shape of the API

1. JS sends a string to native
2. Native sends another string in response

## From JS

```javascript
cordova.exec(function(winParam) {}, function(error) {}, "serviceName", "actionName", ["arg1", "arg2", 42, false...]);
```

* success = the success callback - Gets ?? args
* fail = the failure callback. Gets ?? args
* serviceName = name of Native _class_ to call
* actionName = name of the _method_ within that class to call
* args = the args to pass to that method

    QUESTION: how are types converted? when passing args?

This gets converted into a `CDVInvokedUrlCommand` for native


Plugin JS can add properties to window to allow other JS to communicate

## From Native

* YOu can return many types of messages to the JS
* The thing you return is wrapped in a `CDVPluginResult`
* your class has to extend `CDVPlugin`
* Your plugin runs in the same thread as the UI so will block it if you do a lot of work
    * TODO: investigate background threads in iOS

### CDVPluginResult

* has a number of class methods on it that let you return different types of
* stuff

### CDVInvokedUrlCommand

* wraps the command you get from JS

### Lifecycle methods in the plugin class

* Each insance of the UIWebView gets an instance of the plugin object
    * => there will be a single instance of the plugin class in memory
* plugins are not instantiated until they are first used unless you tell
  `config.xml` to instantiate it on load
* there is no _designated initializer_ for the plugin - just implement the
  `pluginInitialize` method in the class to run logic on start-up

## Public API that the native class can implement

* onReset -
    * run with the UIWebView navigates to a new page or refreshes (which reloads the JS)
* pluginInitialize
    * run when the plugin is initialized
* pause
* resume
* handleOpenURL
* app terminate ??


