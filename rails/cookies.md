# cookies

* Rails will take the contents of the `Cookie` HTTP header and deserialzie it into `cookies` object
* You can read and write properties of that object
* `cookies` object available in
    * controllers
    * templates
    * helpers
* store small bits of non-sensitive info e.g. user preference
* when you write stuff into `cookies` rails will magically serialize it into the `Cookie:` HTTP header when you send the response

```rb
# simple usage
cookies[:some_pref] = false

# detailed usage (setting options)
cookies[:some_pref] = {
    value: false,
    expires: 5.minutes.from_now,
    domain: 'some.com', # use when you want to set cookie for your whole domain, not jsut current host
    path: '/',
    secure: true, # transmit over HTTPS only
    http_only: false, # available to scripting or just HTTP - TODO: ????
}

cookies.delete[:some_pref]
cookies.permenant[:some_pref] = false # auto-sets a 20 year expires

# signed cookie data
cookies.signed[:some_pref]
begin
    cookies[:some_pref]
rescue ActiveSupport::MessageVerifier::InvalidSignature
    # rails will explode if a signed cookie was tampered with
end
```

TODO: go deeper into the how the Cookie HTTP header is implemented

