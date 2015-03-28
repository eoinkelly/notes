
# Cordova versioning

cordova npm package depends on

    dependencies:
    { 'cordova-lib': '4.3.0',
        q: '1.0.1',
        nopt: '3.0.1',
        underscore: '1.7.0' },

cordova-lib depends on

    dependencies:
    { 'bplist-parser': '0.0.6',
        'cordova-js': '3.8.0',
        d8: '0.4.4',
        'dep-graph': '1.1.0',
        elementtree: '0.1.5',
        glob: '4.0.6',
        'init-package-json': '^1.2.0',
        mime: '1.2.11',
        npm: '1.3.4',
        npmconf: '0.1.16',
        osenv: '0.1.0',
        plist: '1.1.0',
        'properties-parser': '0.2.3',
        q: '1.0.1',
        rc: '0.5.2',
        request: '2.47.0',
        semver: '2.0.11',
        shelljs: '0.3.0',
        tar: '1.0.2',
        through2: '0.6.3',
        underscore: '1.7.0',
        unorm: '1.3.3',
        'valid-identifier': '0.0.1',
        xcode: '0.6.7' },

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


