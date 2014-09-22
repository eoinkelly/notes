
## What are the base technologies of offline browser storage?

1. IndexedDB
    * fairly widespread adoption in newest browsers
        * incomplete in IE 10/11
        * will be in safari 8
    * async access
    * much larger storage limits (firefox will ask user at 50 MB), unlimited
    * (with user permission) in chrome
2. WebSQL (is dead spec)
    * there is a cordova plugin that implements the webSQL api using a native
      SQLite impelmentation so this
    * this would allow our cordova apps to have access to a SQL store
      so this spec (although dead in browser) might have some legs yet!
3. localStorage
    * implemented everywhere
    * access is synchronous
    * not available in private browsing mode on ios/android
    * occasionally gets cleared out on ios 5,6
        * ?? does this still happen???
        * ? how much can I depend on localstorage to always be there?


## LevelDB

* is google implementation of IndexedDB
* is used by Google to implement IndexedDB support in Chrome
* can be used as a backend for Riak
* soon to be a backend for MariaDB
* is very fast - outperforms SQLite and Kyoto Cabinet (according to wikipedia)


## LevelUP API

* In node _levelUP_ is an API to datastores
* It came about by splitting the Node LevelDB adapter into two layers
    1. LevelUP
    2. LevelDOWN
* lots of datastores implement the LevelUP API
    * levelDOWN (the bit of levelDB that implements it)
    * Riak
    * Redis
    * Level.js

Level.js
    * browser based implementation of LevelUP API
    * uses IndexedDB under the hood.
    * allows you to use the node LevelUP API in the browser

https://github.com/jensarps/IDBWrapper
    * a x-browser wrapper for IndexedDB

## PouchDB

* provides a high-level
* a possible alternative is lawnchair http://brian.io/lawnchair/

## Appcache

Method of defining web page files to be cached using a cache manifest file,
allowing them to work offline on subsequent visits to the page

* solves a different problem to the storage stuff above
* good x-browser support
* has reputation of being buggy and hard to work with
* http://alistapart.com/article/application-cache-is-a-douchebag

QUESTION: are there newer, better alternatives to this?

# Browser storage technologies

http://www.webdirections.org/blog/webstorage-persistent-client-side-data-storage/

1. cookies
1. local storage
1. session storage

session = while the "top level browsing context" (window or tab) remains open and the URL points at the same "origin" (domain)

To have access to each other’s webStorage, tabs or windows must have the same

1. protocol
1. subdomains
1. top-level domain

### sessionStorage

* stores data during a session and is removed once a session is finished.
* if user goes to another site and navigates back it is still in the same session
* is **not** shared between tabs/windows
* is shared with "sub frames" (? iframes or html frames) on the same page
* some browsers will persist it across a browser crash, some do not

```js

var ss = window.sessionStorage;
if(ss) {
    try {
        // use it
        ss.setItem('key', "value");
        ss.getItem('key');
        ss.getItem('notThere'); // => null
        // returns null if key does not exist
    } catch (e) {
        //test if this is a QUOTA_EXCEEDED_ERR
    }

} else {
    // fallback to cookies or something else
}
```

Gotchas

* All data is stored as strings (so objects must be serialized first)
* JS strings are UTF-16 encoded so each char takes up 2 bytes which effectively halves the available space
* Browsers do not provide reliable access to it during private browsing
    * safari: returns null for everything
    * chrome: lets you read previous values but does not persist localStorage
* webStorage (includes both localStorage and sessionStorage) is synchronous!

Limits

* Spec recommends 5MB per domain (16 byte JS chars means we get 2.5 MB)
    QUESTION: as total for session + local or each?
* `setItem` will throw an error if you try to write past the limit

Events

Only fired when storage is **changed**

QUESTION: is it fired on session, local or both?

```js
window.addEventListener('storage', function(storageArea) {

}, false);
```

* The event is mostly not received in the window/tab that changed the storage -
  it is received in the other open windows/tabs for that domain (some browsers
  do also issue it in window/tab that madt the change)

### localStorage

* localStorage is almost identical to sessionStorage, but the data stored
  persists indefinitely, until removed by the application.

Gotchas

* iOS 5/6 will sometimes just delete it

## TODO:

http://www.webdirections.org/resources/getting-offline-appcache-localstorage-for-html5-apps-that-work-offline-john-allsopp/

QUESTION: is there a viable alternative to app cache

QUESTION: read up more on how to sanely use appcache
    allsop talks about it on webahead
    http://alistapart.com/article/application-cache-is-a-douchebag




