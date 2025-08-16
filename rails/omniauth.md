# Omniauth gem

- defines an API for implementing the details of auth that various ruby classes
  can implement
    - classes which implment this API are called strategies
    - each strategy is
        - a rack middleware

it takes an `origin` param

- `omniauth` gem includes just one "strategy" called `Developer`

- `OmniAuth::Builder` is a special strategy (rack middleware)
    - it allows you to define a collection of strategies, the user just has to
      pick one

1. Redirect users to /auth/:provider where provider is the name of a strategy
1. Omniauth receives the redirect and sends the user to the provider and gets
   the response
1. Omniauth redirects to `/auth/:provider/callback` which you should set to a
   controller action that you wrote in your app
    - Omniauth sets the "Authentication hash" in the rack env i.e.
      `request.env['omniauth.auth']`
        - the hash contains all the info omniauth was able to get from the
          provider
    - you should pull the info out of the hash and do whatever is necessary to
      sign-in the user

Don't forget about the Omniauth CVE

Omniauth gem depends on hashie, ruby 2.2+

Singleton _ create instances via Klass.instance not Klass.new _ ensures that
only one instance is ever created (all calls to Klass.instance return the smae
method

Module#autoload(:str_or_sym, "filepath") tells ruby to `require "filepath"` the
first time :str_or_sym is accessed by any ruby code _ it delays requiring the
code until it is used _ why bother? \* it lets your app boot faster because the
activitiy of requring the file is delayed

module_function

- makes a copy of the subsequent methods onto the module itself
    - this means the methods are
        1. instance methods into whatever class the module is mixed into
        2. class methods on the module itself

request_phase must be implemented by the strategy

- callback_phase method
    - implemented by the base strategy class so custom strategy does not have to
    - does:
        1. build an instance of AuthHash by pulling in the return values of the
           `info`, `extra` and `credentials` methods
        1. puts the auth hash in env['omniauth.auth']
        1. Call the next app in the rack chain

```ruby
OmniAuth.strategies
OmniAuth.config

# the utils are defined with module function
OmniAuth::Utils.form_css
```

Handling failure

`OmniAuth:Strategy.fail!(message_key, exception = nil)` is how omniauth triggers
a failure it puts the following in the env:

      env['omniauth.error'] = exception
      env['omniauth.error.type'] = message_key.to_sym
      env['omniauth.error.strategy'] = self

and logs the error and then calls the `on_failure` rack app

The default `on_failure` rack app is `OmniAuth::FailureEndpoint` but you can
provide your own

`OmniAuth::FailureEndpoint` _ will raise exceptions in development (you can
config which envs this happens in) _ otherwise redirects to
`/auth/failure?message=#{message_key}{origin_query_param}{strategy_query_param}`
