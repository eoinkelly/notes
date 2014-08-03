# UIWebView

_IIRC these are notes on the new webview in iOS 8+_

* has a `delegate` property
    * which is sent messages while the content is loading
    * can implement methods to talk to the UIWebView instance

* Ancestors: `UIView < UIResponder < NSObject`
* part of UIKit "framework" `#import UIKit` to get it
* conforms to 7 interfaces. These are ???


## Plugins

One instance of a plugin object is created for the life of each UIWebView

Plugins are ordinarily instantiated when first referenced by a call from
JavaScript. Otherwise they can be instantiated by setting a param named onload
to true in the config.xml file

* methods to implement (where???)
  * onReset - called when the webview is refreshed or moves to different screen
  * pluginInitialize - called when your plugin is initailzied action
      * this is what the javascript exec() will hit in your class

* the pattern is that JS initiates all work done on the iOS side

* messages from JS -> iOS
  * sent to the action method of the given plugin class
  * can have arbitrary num of args

* messages from iOS -> JS
  * You can use CDVPluginResult to return a variety of result types back to the
    JavaScript callbacks, using class methods
  * You can create String, Int, Double, Bool, Array, Dictionary, ArrayBuffer,
    and Multipart types. You can also leave out any arguments to send a status, or
    return an error, or even choose not to send any plugin result, in which case
    neither callback fires.
  * you seem to return an instance of CDVpluginResult  that encapsulates the response


There doesn't seem to be a way for the iOS to send a message to JS without the
JS polling ???
