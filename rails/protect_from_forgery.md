
# Rails protect_from_forgery

If you are authenticating with a cookie, you need CSRF protection!

All cookies for website AAA are sent with every request to AAA, even if that request originated on a page served by BBB.

## Overview

* https://api.rubyonrails.org/classes/ActionController/RequestForgeryProtection/ClassMethods.html
* Bear in mind that GET and HEAD requests are not checked.

## Background: CSRF checking

* https://guides.rubyonrails.org/security.html#cross-site-request-forgery-csrf
* https://cheatsheetseries.owasp.org/cheatsheets/Cross-Site_Request_Forgery_Prevention_Cheat_Sheet.html
* https://medium.com/rubyinside/a-deep-dive-into-csrf-protection-in-rails-19fa0a42c0ef

* attacker can make the victim perform an action on our site without the victim's knowledge.
    * The scope of the badness is limited to what the victim can do on the site
    * usually they can be tricked into damaging their account in some way

Synchronizer token pattern

1. Server generates a token for each response or a single token for the whole session
    * per response tokens can break back button in browsers
    * per session tokens, the token value is stored in the session and used for all requests in the session
    which does rails do?
    * Tokens must be random and long enough to be too hard to guess
    * Session tokens must be secret
1. User submits request
1. Rails compares the token in the session (which is encrypted and presumed inaccessible to the client) to the token submitted by the form as a hidden field
1. If tokens match all good, if not, take one of the actions efined by `protect_from_forgery`


CSRF for JSON rquests can be done by replacing the hidden field in the form with a field in the JSON
    how does rails learn which field to check? needa custom strategy?


> rails will appear to generate a new CSRF token on every request, but it will
> accept any generated token from that session. In reality, it is just masking
> a single token using a one-time pad per request, in order to protect against
> SSL BREACH attack. More details at
> https://stackoverflow.com/a/49783739/2016618. You don’t need to track/store
> these tokens.
>
> source: https://stackoverflow.com/a/50225227/473040

Rails will check the X-CSRF-Token header

Aside Q: do browsers support real PUT PATCH DELETE yet?

> By default, Rails includes an unobtrusive scripting adapter, which adds a
> header called X-CSRF-Token with the security token on every non-GET Ajax
> call. Without this header, non-GET Ajax requests won't be accepted by Rails.

### Why does rails set both a csrf-param and csrf-token in the header?

`csrf-param` is the name of the hidden field in each form on the page. It is used to update "cached" forms in the DOM with a new `csrf-token` value.


Rails UJS [has a function](https://github.com/rails/rails/blob/129fe98f4cc74be53c7d194ff2faf06d9b382c41/actionview/app/javascript/rails-ujs/utils/csrf.js#L22) which will read both `csrf-param` and `csrf-token` from `<head>` and update each form on the page with the new token.

```
<meta name="csrf-param" content="authenticity_token">
<meta name="csrf-token" content="gRJcP33JrcKR0OHd-i5wNDmM-tv5a2NW6wFD7dOgqCNFq20jj-cwJ-i6M4-AdoFpQDjLhbZXmUfS1EUqbdEP5g">
```

### Does rails automatically set X-CSRF header for xhr/fetch requests?

Rails-ujs does. See
https://github.com/rails/rails/blob/129fe98f4cc74be53c7d194ff2faf06d9b382c41/actionview/app/javascript/rails-ujs/utils/csrf.js#L16

Turbo uses `csrf-param` as the name of the value to lookup in the cookie to
find the token. If that doesn't exist, it falls back to the `csrf-token` meta
tag value. See https://github.com/hotwired/turbo-rails/blob/d1dd2962a047c8d84f62590c7e8eba4458ea3ec8/app/assets/javascripts/turbo.js#L765

Q: Why does it try to find it in the cookie first?

### How often does Rails update the CSRF token

* Rails sets one CSRF token per session
* The `csrf-token` put into HTML (both `<head>` and each `<form>` element on the page) changes each time because it is a _masked token_
* The masked token is created by
    1. Create a random pad (string) same length as token
    1. XOR the one-time pad and the token together to get an "encrypted" token
    1. Prepend the one-time pad to the "encrypted" token string
    1. Base64 encode the output of the previous step
* Expressed in code
    ```ruby
    # how Rails masks the real token
    # https://github.com/rails/rails/blob/main/actionpack/lib/action_controller/metal/request_forgery_protection.rb#L522

    one_time_pad = SecureRandom.random_bytes(AUTHENTICITY_TOKEN_LENGTH)
    encrypted_csrf_token = xor_byte_strings(one_time_pad, raw_token)
    masked_token = one_time_pad + encrypted_csrf_token
    Base64.strict_encode64(masked_token)
    ```
* You can easily get back to the real token by reversing the "encryption" so why bother?
    * Token masking is performed to protect against SSL BREACH timing attack.
        * > the purpose of the XORing is to change the CSRF token on every request so an attacker can't use timing attacks to discern the secret
        * SSL BREACH details: https://breachattack.com/resources/BREACH%20-%20SSL,%20gone%20in%2030%20seconds.pdf


### Does the csrf-token in HTML head match what is put as hidden field in forms?

Yes. Both HTML head and hidden form fields get the same masked token (see above about masking).

### protect_from_forgery automatically opts you into verify_same_origin_request after_action

`verify_same_origin_request` check that the browser did a same-origin check, it doesn't chech same-origin because it (obviously) can't.

Rails `verify_same_origin_request` after action will fail if the response is JS and the request was not XHR

Rails knows that browsers will use the same-origin policy for XHR and Fetch requests but not for regular HTTP requests. So if the request is asking for JS and isn't XHR/Fetch then the same-origin check never happened



## The check

boils down to:

```ruby
  before_action :verify_authenticity_token, options
  append_after_action :verify_same_origin_request
```

What does it actually do

>  If protect_from_forgery is enabled on an action, this before_action flags its after_action to verify that JavaScript responses are for XHR requests, ensuring they follow the browser's same-origin policy.


      # * Is it a GET or HEAD request? GETs should be safe and idempotent
      # * Does the form_authenticity_token match the given token value from the params?
      # * Does the +X-CSRF-Token+ header match the form_authenticity_token?

Consequences

* If your GET or HEAD makes data changes then this protection will not help you! Make sure GET/HEAD are idempotent



## The action on a failed check

If the check happens and it fails then one of three things can happen

1. `:null_session` - makes the session empty during this request only (default behaviour)
    * Consequences
        * no user id so the user is not signed in - it becomes an unathenticated request
        * Future requests are unaffected.
1. `:exception` - Rails raises an `ActionController::InvalidAuthenticityToken` exception
    * Consequences
        * Rails refuses to process the request. Future requests are unaffected.
1. `:reset_session` - resets the session
    * ??? emptys the session for this and all future requests?
    * Consequences
        * no user id so the user is not signed in - the request becomes an unathenticated request
        * Future requests are also impacted if the browser updates the session cookie we send back
            * The user will be signed out
1. You can implement a custom strategy class (see docs)


## skip_forgery_protection

Just a wrapper for

```ruby
skip_before_action :verify_authenticity_token
```

You can pass any of the `skip_before_action` options to `skip_forgery_protection`
