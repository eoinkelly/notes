
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
* is used by Google to implement IndexedDB support in Chrom
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
## Conclusions

PouchDB seems the most interesting in the browser

## TODO:

* listen to web ahead offline eipsode
* spend a pomodoro digging into indexeddb directly
    * ? is it usable without a wrapper?
