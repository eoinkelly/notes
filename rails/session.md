# Session

"session" means two things

1. The data structure we store to represent the current user
2. the time that the user is actively using the application

Characteristics of `session` (the data structure rails creates for us)

* Accessed via `session` object which is available in
    1. controllers
    2. views
    3. models
    4. helpers
* a "hash like" object
* sessions are lazy loaded (if you don't refer to `session` then it is not loaded)
* By default they are stored in the cookie sent to/from browser so you have 4KB serialized limit

## Do and don't of putting stuff in session

* Don't use session for persistent storage - the DB is for that
* Don't store full objects in there, store Ids
* Don't store critical data in there - always be able to to lose the session without it causing a problem
* Don't store objects that you will modify the sturcture of in there
    * it will be very hard to clear out the old ones when you upgrade code especially if using cookie storage
    * Use a deployment script to clean out old sessions
* Rails will set a cookie with a unique id that is used to lookup a particular users' session in the "session store"

## Ways the session is cleared

1. user closes browser
2. user clears cookies
3. You change the secret string used to hash session data
4. You change the digest algorithm used to hash session data
5. You call `reset_session` in your code which will clear the session from the cookie sent with the next response

## Typical contents of a session structure

* Id of current user
* Flash message

### storage options

* Cookie store
    * Default in rails
    * ++ dramatically faster than alternatives
    * -- 4KB serialized data limit
    * ++ cookies are encrypted & signed in rails4 (were only signed in rails3)
    * -- vulnerable to replay attacks e.g. if you store how many "credits" a
      user has in cookie they can spend credits and then get old amount back by
      using old cookie
        * migitation: **do not store stuff in session that you would not be ok with the user seeing**
        * can be mitigated with nonces but this is much harder to coordinate
          across multiple processses serving your app (you end up hitting the
          DB every time again)
    * -- malicious user could steal a user's computer and use cookie to appear like they were still logged in.
* DB using ActiveRecord
    * activerecord-session_store gem does this in Rails 4
    * -- it hits the DB every time a session is read!
    * -- you have to vacuum up old sessions sometimes as user may just close browser or clear cookies
* Memcached
    * Use dalli gem (better than memcache-client gem)
    * ++ built-in expiration


## Relevant Files

* `config/initializers/session_store.rb`
    * decides which kind of storage to use
* `config/initializers/secret_token.rb`
    * the key used to encrypt the session

## Serialized session data format

    (TODO: this is just a guess)
    {session data}{SHA1 checksum of session data} --> |encryptor> --> {encrypted session}

## Sources

See chap 13 Rails 4 way

## QUESTION: how do sessions "expire"?

* You can configure how soon sessions will "expire" in rails via
    ```rb
    Yourapp::Application.config.session_store :cookie_store,
                                            :key => "_yourapp_session",
                                            :expire_after => 2.minutes
    ```
* Notice that expiry is set on the "session store", not the "session" itself
    * So default session expiry depends on how cookies implement expiry
* By default rails does not expire sessions
* You can manually expire a session using `session[:last_seen]` to test how
  long it has been since the session was seen and then `reset_session` to
  "expire" it.

QUESTION: ??? how do we ensure that "logged out" users cannot get back in by just providing the old cookie ???
QUESTION: how do I manually clear out old cookies or expire sessions?


