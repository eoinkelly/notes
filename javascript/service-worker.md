# Service worker

Service worker code

* has no DOM access
* has a completely different lifecycle to the webpage
* may be stopped and started by the browser - if you need persistence between function calls you need to use localstorage or similar

Lifecycle of a SW

1. Register it
    * On the window.load event, you call `navifator.servicWorker.register('path/to/sw-code.js').then(function(registration) { })`
        * QUESTION: can it be done at documentReady ?

1. add event listeners for
    * install - fired once when the SW code is loaded by browser
        * also fired whenever a new version of the SW JS file is downloaded
    * fetch - fired every time the browser fetches a resource
    * waiting
    * activate - fired when the new version of the SW is about to take over from the old version


Statement: Other than the code which registers the SW, the rest of your app code shouldn't know or care that the SW exists and is helping with caching.
    is this true?


