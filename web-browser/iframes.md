# iframes

Embed another document within a HTML document

* In CSS they are `display: inline` by default
* They are not a viable way of doing multi-threading with JS
    * Most browsers seem to run them on same thread as the containing document


## Limitations

* can only draw within its frame
* no access to the outer DOM
* no access to the outer page local storage

## Sandbox attribute

Has a `sandbox` attribute which whitelists capabilities

`sandbox=""` (or `sandbox`) makes it as secure as pssible

* sandboxed documents can _never_ run plugins (as plugin code is native, unsandboxed)

Capabilities you can whitelist:

1. allow-forms
    * allows the iframe to submit forms
2. allow-popups
    * allow the iframe to create new windows
3. allow-pointer-lock
    * allows it to access the [Pointer Lock API]() which can constrain mouse movement
    * Pointer lock has very poor browser support
4. allow-same-origin
    * allows the iframe document to retain its origin
    * e.g. if you load an iframe from twitter.com and allow-same-origin is set
      then that iframe will have access to all the cookies and stored data in
      your browser from twitter.com. Without this the browser would force the
      iframe into a new origin which would cut off access.
5. allow-scripts
    * allow JS execution
6. allow-top-navigation
    * allow the iframe document to break out of the iframe by navigating the top-level window.
    * without this setting `window.top.location = '...'` would throw exception

```
<iframe sandbox="allow-same-origin allow-scripts allow-popups allow-forms"
    src="https://platform.twitter.com/widgets/tweet_button.html"></iframe>
```

Has really good support http://caniuse.com/#feat=iframe-sandbox except not in IE9 and older.

## postMessage API

https://developer.mozilla.org/en-US/docs/Web/API/Window.postMessage

* Allows sending of data between two windows/iframes _across domains_
* e.g. `otherWindow.postMessage(message, targetDomain)`
* it will do some automatic serialization of JS objects so you don't necessairly have to explicitly serialize/deserialize

```js
// in containing document
window.addEventListener('message', function(e) {
    var frame = document.getElementById('my-frame');

    // check that the data really is from our iframe
    if (e.origin === null && e.source === frame.contentWindow) {
        // do stuff
    }
});

```

```js
// in iframe document

window.addEventListener('message', function(e) {
    var outerWindow = e.source
    var data = e.data;
    // do stuff ...


    // send back our result
    outerWindow.postMessage('some reply', e.origin);
});
```
