# Xcode Archives

Xcode creates archives and these are what you releas to the store.

An general archive is a timestamped bundle with:

- An "install style" build of your app
- The applications debug symbols ina separate `.dSYM` file
    - These can be used to symbolicate crash logs
- Verification and submissions status for the app
- your own comments

An "application archive" is a particular kind of archive that contains nothing
but the application - only _application arichves_ can be submitted to the store.

It is a common problem that frameworks and libraries will, as well as linking
with the app, be copied into the archive

To release your app on the store you have to embed both libraries and
frameworks.

- Static libs are always embedded in your app
- Use a "Copy files" step to copy in frameworks and dynamic libraries into the
  app
