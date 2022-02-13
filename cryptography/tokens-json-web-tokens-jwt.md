# JSON Web Tokens

Sources

* https://jwt.io/
* https://tools.ietf.org/html/rfc7519

Overview

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
1. Instead of getting a cookie HTTP header containing the ID of a session on the server, they get a JWT
1. The browser stores the JWT in LocalStorage (or as a cookie)
1. The browser wants to access a protected resource
    1. It adds the JWT to the request in an `Authorization: Bearer <JWT>` HTTP header
1. The server receives the request, verifies the JWT and sends the response
    * the server did not have to look up a session in a DB or other shared storage
    * JWT contains all the info the sever needs for the session
    * I guess the JWT contains the entire session?

## Libraries

### Ruby

https://github.com/jwt/ruby-jwt

```ruby

require "jwt"

hmac_secret = 'long key' # this must be long for security. recommend using `rails secret` output length

token = JWT.encode(payload, hmac_secret, "HS256")
puts token

# notice we are telling the decoder which algorithm to use. This is important.
# the lib should never trust the `alg` header from the token!
decoded = JWT.decode(raw_token, hmac_secret, true, { algorithm: "HS256"})
puts decoded
```

### Other langs

https://jwt.io/libraries

## Security issues

* Following the standard does not prevent you from implementing an insecure system
* In cryptography spaces, a message **signature** is not the same as a message **digest**. The JSON spec uses the words interchangeably

Security problems:

1. Weak algorithms
    * You can choose 'None' as your algorithm.
    * Some implementations will default to choosing the algorithm to verify the digest from the header so the sender can send 'None'
    * This was probably added to help with testing but it's a foot gun
    * Some of the available public-key algorithms have known issues e.g. RSA + PKCSv1.5 or ECDSA
1. HS256 is only good if you use a sufficiently long shared secret
    * If your shared secret can be brute forced or otherwise cracked then your security is broken
    * HS256 is HMAC + SHA-256
1. Algorithm confusion
    * Good summary: https://auth0.com/blog/critical-vulnerabilities-in-json-web-token-libraries/
    * Server is setup to expect an asymmetric key algorithm e.g. RSA to verify the token
    * Steps
        1. Submit token with alg=HS256 (i.e. a symmetric algo), and signed with the servers public RSA key
        1. Server looks at alg and decides the signature is symmetric but still uses it's copy of it's own public key to verify the sig.
        1. The sig passes.
    * The problem is that the server has to trust the `alg` field **before** it has verified that the message is legit. The server has to allow the attackers to choose how it verifies the token.
    * To avoid:
        * The JWT lib should always let your code pass in the name of the algorithm to use for the sig. If this isn't there then the lib must be trusting the token which is bad.
        * The lib should never trust the `alg` field.
        * **Always whitelist your algorithms**

