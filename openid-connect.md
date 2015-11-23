# OpenID Connect (OIDC)

* Based on OAuth 2.0
* OpenID Connect is effectively version 3 of the OpenID specification
* OIDC assumes you’ll identify people using email addresses
    * OpenID 2 assumed a URL
* in practice there are a few big OIDC ID providers (IDP) at the moment
    * Microsoft
    * Salesforce
    * Google
* Facebook is an identity provider but does not use OIDC
* OIDC is a "framework"
* a simple identity layer on top of the OAuth 2.0 protocol
* replaces OpenID 2.0 protocol
* the client is called the "relying party"
* the identity provider is the IDP or the "Open ID Connect Provider" OP
* wants apps to redirect users to a real browser for auth - it does not recommend webviews as they can be snooped
    * -- is quite "browser-centric" in its world view
* OIDC says nothing about *how* a user should be authenticated - that is up to the IDP
* Client apps receive the user’s identity encoded in a secure JSON Web Token (JWT), called ID token.
* Clients use OAuth 2.0 flows to obtain ID tokens
    * scope in the OAuth requests contains `openid` which indicates that the client wants access to their identity token
* Ruby lib: https://github.com/nov/openid_connect
* Client does the normal OAuth2 thing to get the authorization code and then you exchange the code for an access token AND the users identity token
* Client should then undo the base64 encoding and validate the token

```
// example of a google identity token
{"iss":"accounts.google.com",
 "at_hash":"HK6E_P6Dh8Y93mRNtsDB1Q",
 "email_verified":"true",
 "sub":"10769150350006150715113082367",
 "azp":"1234987819200.apps.googleusercontent.com",
 "email":"jsmith@example.com",
 "aud":"1234987819200.apps.googleusercontent.com",
 "iat":1353601026,
 "exp":1353604926,
 "hd":"example.com" }
```

## JSON Web Tokens (Identity token)

* The _Identity token_ is a JWT
* a compact, URL safe way of transferring "claims" between two parties
* just to be confusing, OAuth2 has a Json Web Token _profile_ that allows you to use JWTs with OAuth 2
* format is JWT (JSON)
* http://tools.ietf.org/html/rfc7519
* represents an "identity card"
* to get an identity token the client sends the user to their IDP with an authorization request
* required fields shown below but identity tokens may also contain
    * subject name
    * email address
* emails are not suitable for use as unique ids for the user - you should use the `sub` key instead
* is always digitally signed by the IDP
* may be encrypted
* the whole signed (and optionally encrypted) token is base64 encoded so it can be embedded in a URL

```
// an identity token contains at least:
{
  "sub"       : "alice",                    // subject, unique id of the user
  "iss"       : "https://openid.c2id.com",  // issuing authority
  "aud"       : "client-12345",             // audience
  "nonce"     : "n-0S6_WzA2Mj",             // optional
  "auth_time" : 1311280969,                 // when the user was authenticated
  "acr"       : "c2id.loa.hisec"            // how strongly the user was authenticated
  "iat"       : 1311280970,                 // issue date
  "exp"       : 1311281970,                 // expiry date
}
```

## Discovery document

* A JSON document at a well known location hosted by an IDP that tells you all the URLs you need to use OpenID
* examples
    * google https://accounts.google.com/.well-known/openid-configuration
    * Salesforce https://login.salesforce.com/.well-known/openid-configuration
    * Micorsoft Azure https://login.windows.net/common/.well-known/openid-configuration
    * Facebook does not use OIDC - it uses its own extension to OAuth instead
* The idea is that an OIDC client library would hard-code the discovery
  document URL only and use it to discover all the other URLs it needed

```
// https://accounts.google.com/.well-known/openid-configuration
{
    "authorization_endpoint": "https://accounts.google.com/o/oauth2/v2/auth",
    "claims_supported": [
        "aud",
        "email",
        "email_verified",
        "exp",
        "family_name",
        "given_name",
        "iat",
        "iss",
        "locale",
        "name",
        "picture",
        "sub"
    ],
    "id_token_signing_alg_values_supported": [
        "RS256"
    ],
    "issuer": "https://accounts.google.com",
    "jwks_uri": "https://www.googleapis.com/oauth2/v3/certs",
    "response_types_supported": [
        "code",
        "token",
        "id_token",
        "code token",
        "code id_token",
        "token id_token",
        "code token id_token",
        "none"
    ],
    "revocation_endpoint": "https://accounts.google.com/o/oauth2/revoke",
    "scopes_supported": [
        "openid",
        "email",
        "profile"
    ],
    "subject_types_supported": [
        "public"
    ],
    "token_endpoint": "https://www.googleapis.com/oauth2/v4/token",
    "token_endpoint_auth_methods_supported": [
        "client_secret_post",
        "client_secret_basic"
    ],
    "userinfo_endpoint": "https://www.googleapis.com/oauth2/v3/userinfo"
}
```


## Aside: JSON Web Keys

* A JSON data structure to represent a cryptographic key
* https://tools.ietf.org/html/rfc7517
* basically it specifies the property names for key data with a bunch of metadata that describes the key data

```
// example of a JWK
{
    "kty":"EC", // key type: Elliptic curve
    "crv":"P-256", // algorithm
    "x":"f83OJ3D2xF1Bg8vub9tLe1gHMzV76e8Tus9uPHvRVEU",
    "y":"x_FEzRu9m36HLN_tue659LNpXW6pCyStikYjKIWI5a0",
    "kid":"Public key used in JWS spec Appendix A.3 example" // key id
}
```
