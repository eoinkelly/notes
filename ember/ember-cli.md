# What are the advantages of using an addon vs pulling stuff in directly yourself?

++ convenience - bit like gems in rails land ++ it is basically just a curated
set of npm packages that you know will work and have some of the integration
work done for you e.g. pulling in 'moment' direclty from npm vs pulling in
'ember-cli-moment'

-- in many cases they don't seem to do a great deal -- I don't understand what
they are doing so makes me not trust them fully ++ they do provide a good
reference for what to do - it is worth learning how they do it so we can
streamline pulling in packages

Conclusion: the main barrier to using it is that I don't know what they are
doing

# Ember-cli 101 book

Express.js has middlewares that can be used for tricks

- tests are run by Testem
    - can run in "CI" or "development" mode
- compiles down to AMD modules
- files are named using "kebab case" so-they-look-like-this.thing
- you can prefix apps with their type e.g. `/routes/index.js` or
  `routes/route-index.js` are acceptable

# Watchman

- runs as a daemon
- watches for changes in "roots" (directory heirarchies)
- treats symlinks as ordinary files, does not follow them
- you can "subscribe" to file changes which occur under a "root"
- is smart enough to wait for a root to settle down if it changes quickly
