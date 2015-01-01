# Xcode Archives

Xcode creates archives and these are what you releas to the store. An archive is a timestamped bundle with:

* An "install style" build of your app
* The applications debug symbols ina  separate `.dSYM` file
    * These can be used to symbolicate crash logs
* Verification and submissions status for the app
* your own comments
