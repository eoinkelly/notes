
I want a solution for a good offline experience for my app

# tools

* orbit.js
    * ember-orbit
    * ++ does provide multiple data stores
* indexed-db
    https://github.com/kurko/ember-indexeddb-adapter
    * -- doesn't provide online + offline feature
* pouch-db
    offers something called pouch-db server ???
* localforage
    * from mozilla
        > Offline storage, improved. Wraps IndexedDB, WebSQL, or localStorage
        > using a simple but powerful API.
    * https://github.com/genkgo/ember-localforage-adapter
    * -- doesn't provide online + offline feature
* firebase
    * no offline support OOTB
    * ++ easy to setup

# optoins

use the offline store as my primary store and only occasionaly sync with server (when it can)
    this would allow localforage to work
