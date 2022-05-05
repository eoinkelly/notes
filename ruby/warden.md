# Warden gem

* Lets you do authentication in Rack
* is a rack middleware called `Warden::Manager`
* must appear **after** session middleware (warden will store stuff in the session middleware)
    * anything that stores a hash like object in `request.env['rack.session']` counts as a session middleware
* injects a "lazy object" at `request.env['warden']`
    * the object can do the authentication but something later in the rack graph has to actually trigger it
    * if you don't want to authenticate a request then just don't call `env['warden']`
* warden authenticates a single request
* the actual work of authenticating happens in a "strategy"
* within a strategy, failure is signaled by `throw(:warden)`
    * this will unroll the stack back into the warden middleware which will then send the request to a "failure rack application" which you must create
* warden scopes let multiple users be logged in at the same time
* warden defines callbacks that you can register code for - these callbacks are triggered at various points in the auth cycle
* warden setup steps
    1. ensure it downstream of the session middleware
    2. define a failure application
    3. declare which strategies you want to use
* scopes
    * you are always in a scope
    * these are like profiles for how authentication should work
        * which strategies to use
        * whether to store the user in the session
    * you can have as many as you need e.g.
        * requests matching the admin scope check for admins the db, then in $somewhere_else
        * reqeusts matching the api scope look for a basic auth header
        * requests matching frontend do no auth
* callbacks
    * examples
        * you provide blocks for how to serialise and deserialize user into the session
* looks like this in th rails middleware stack
    * `use Warden::Manager`
        * this will inject the `env['warden']` but not actually do any authenticating?
* most people use warden through devise
    * clearance gem does not use warden

```ruby
# less typing
env = request.env

# has this request already been authenticated
env['warden'].authenticated?

# has this request been authenticated for the :foo scope
env['warden'].authenticated?(:foo)

# try to authenticate with all the defined strategies in turn, if one fails go to next one, if all fail continue
env['warden'].authenticate

# **try** to authenticate with the :password strategy. if fail then just continue
env['warden'].authenticate(:password)

# **try** to authenticate with the :password strategy but die if it fails
env['warden'].authenticate!(:password)

# this returns non nil if authentication worked (you must first call #authenticate)
env['warden'].user


# within any downstream code, you can indicate an authentication failure
throw(:warden)
throw(:warden, something: "someval")
```

## A strategy

* an object which implements
    * #valid?
        * optional
        * if defined and returns false, this strategy will be skipped
        * lets you skip this strategy based on some testing of the request state
    * #authenticate
        * does what it says
        * a lot of stuff is magically available within this method
* are declared with a label and referred to by that symbol in Warden
* warden creators thought that in some cases, shared strategies would be used
    * this has mostly worked out but devs don't tend to think about the underlying warden
    * there is a collection of strategies in https://github.com/hassox/warden_strategies
* Things available within `#authenticate!`:
    ```
    request = Rack::Request object
    session = the session object
    params = request params
    env = the Rack env object

    methods available

    halt! = stop processing strategies
    pass = ignore this strategy (not required but can make things clearer)
    success!(user_object) = login the user
    fail! = fail this strategy and then call halt!
    redirect! redirect to another URL - calls halt!
    custom! = return custom rack array (aka a raw resposne), causes a halt!

    header = set headers in the response
    errors = put your errors in here
    ```