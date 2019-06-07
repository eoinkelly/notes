# JSON Web Tokens

https://tools.ietf.org/html/rfc7519

* a digitally signed JSON object
* are URL safe
* JWTs can be signed and encrypted if you need it
* Often transmitted in the `Authorization: Bearer <token>`

Structure

* Three parts
    1. Header
    1. Payload
    1. Signature
* Parts are separated by `.`
* Signature options
    1. HMAC algorithm + secret
    2. RSA public + private keypair

Use case: Authorization

1. The user authenticates as normal
1. Instead of getting a cookie containing the ID of a session on the server, they get a JWT
    how?
1. The browser stores the JWT in localstorage (or as a cookie)
1. The browser wants to access a protected resource
    1. It adds the JWT to the request in an `Authorization: Bearer <JWT>` HTTP header
1. The server receives the request, verifies the JWT and sends the response
    * the server did not have to look up a session in a DB or other shared storage
    * JWT contains all the info the sever needs for the session
    * I guess the JWT contains the entire session?

```ruby
# easy version
require "jwt"

JWT.decode(raw_token, nil, false)

# more basic version
require "json"
require "base64"

def decode_token(raw_token)
    raw_header, raw_body, raw_sig = raw_token.split(".")

    header = JSON.parse(Base64.decode64(raw_header))
    body = JSON.parse(Base64.decode64(raw_body))
    sig = Base64.decode64(raw_sig)
    # TODO: check the sig

    [header, body]
end
```

