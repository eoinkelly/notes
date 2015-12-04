# OAuth

* allows you to grant 3rd party access to your web resources without sharing their passwords
* is an _authorization_ protocol not an _authentication_ protocol
* NOTE: you cannot run OAuth without SSL and expect it to be secure because you
  have to send the "token" and the "secret" to the client somehow

* OAuth is really for delegation not authorization - maybe should have been called "ODelegate"
    * delegates authorization to a authorization provider

## Sources

* http://oauth.net/
* OAuth 1 guide: http://hueniverse.com/OAuth/guide/
* OAuth 2 RFC http://www.rfc-base.org/txt/rfc-6749.txt

## Version 1

* OAuth 1 = RFC 5849
* deprecated
* largely based on
    * Flicker API Auth
    * Google AuthSub
* -- requires crpyto capability on client-side
* -- did not scale particularly well
    * -- requires state management across setps
    * -- issues long lasting credentials (less secure, harder to sync across data centers)
* -- required endpoints to have access to both the "client credentials" and
  "token credentials" which prevents enterprises from having a centralized
  authorization server to issue credentials and a separate server for API calls
* Methods for obtaining access tokens are called "flows"
* OAuth 1.0 has 3 flows:
    1. web based apps
    2. desktop clients
    3. mobile/limited devices
  but they get merged into one which in practice only worked well for web based apps.

## OAuth 2

RFC 6749 http://tools.ietf.org/html/rfc6749

* OAuth 2 is not backwards compat with 1
* 2 is more of a framework than a protocol
* ++ does not require clients to have cryptography
* ++ 2.0 signatures are much less complicated
* 2.0 access tokens are "short lived"
    * server can issue short lived "access token" and a long-lived "refresh
      token"
    * the client can use the "refresh token" to obtain a new access token
      without involving the user again but still keep the tokens limited
* has better support for non browser based applications
    * 1.0 required apps to open a browser to complete auth
* the "authorization server" (thing that issues tokens) and the resource server
  handling API calls are two distinct roles
    * in many cases both roles are on the same machine but they don't have to
      be (allows scaling)

Problems that OAuth 2 solves:

* I want to provide access to some restricted resource that I own to 1 or more
  third-parties without giving them my credentials for that resource.
* examples:
    * I own a git repo on github.com and I want to give apps (e.g. CI, Slack,
      Gitter etc.) access to it.
* In the simple situation where I give third parties my username and password I
  am functioning as two roles:
    1. authorization granter
    2. the application (third party) that wants access to the resource

It defines 4 roles

1. Resource owner (User)
    * A _user_ who grants access to an _application_ to access their account
    * Each "access grant" has a limited scope
        * scope examples: read only, read/write
1. Resource server
    * hosts the protected resources e.g. photos, git repo
1. Authorization server
    * verifies the idenity of the _user_ and issues access tokens to the _client_
    * In many/most cases a single server will play both the authorization
      server role and the resource server role.
        * In this case this combined role is called the "Service role" or "API role"
1. Client (Application)
    * The _application_ that wants to access the user's account

There are roughly three "phases" (my term) in OAuth 2

1. Get an "authorization grant"
2. Use the "authorization grant" to get an "access token" (sometimes this is
   rolled into phase 1)
3. Use the "access token" to get the protected resource.

### Authorization grant

* is a credential
* is also kind of a code flow
    - ??? not sure how these 4 auth grant types intersect with the 7 flows ???
* represents the resource owner's authorization to the client application to
  access the protected resource
* is an abstraction over the idea of granting authorization
    * in some cases the grant is an authorization code that allows you to get
      an access token which allows you to get the protected resource
    * in some cases it is an access token that allows you to get the protected
      resource
* ??? is the authorization grant the same as a refresh token ???
* There are four defined grant types:
    1. authorization code grant type
        * is an "intermediate credential"
        * flow
            1. client-app begins authentication by redirecting user to the authorization server
                * parameters (embedded in the URI that the user is redirected to)
                    * response_type=code (required)
                    * client_id (required)
                    * redirect_uri (optional)
                    * scope (optional)
                    * state (recommended)
                        * used to prevent CSRF (see section 10.12) TODO
                        * client-app can send an opaque "state" parameter with
                          this redirect.
                        * The authorization server must pass this parameter
                          back to the client-app when the user has finished.
                * example
                ```
                GET /authorize?response_type=code&client_id=s6BhdRkqt3&state=xyz
                        &redirect_uri=https%3A%2F%2Fclient%2Eexample%2Ecom%2Fcb HTTP/1.1
                Host: server.example.com
                ```
            1. authorization server checks that the request is valid
            1. user authenticates themselves with authorization server
            1. user chooses what kind of access to give the client-app to their
               protected resources
            1. authorization server creates an "authorization code" that
               represents this granting of access
            1. auth server redirects user back to the `redirect_uri` it got from the client-app
                * parameters (appended to the redirect URI)
                    * code (required)
                        * the authorization code
                        * recommended that this code
                            * only remain valid for 10mins max
                            * can only be used once - if client tries to use it
                              more than once the authorization server should
                              revoke all tokens issued based on that code
                    * state (recommended)
                        * the state pararameter received from the client-app
                    * example
                    ```
                    HTTP/1.1 302 Found
                    Location: https://client.example.com/cb?code=SplxlOBeZQQYbYS6WxSbIA&state=xyz
                    ```
                * any errors in the process are communicated back to the client via an error response e.g.
                  ```
                  HTTP/1.1 302 Found
                  Location: https://client.example.com/cb?error=access_denied&state=xyz
                  ```
                  * paramenters
                    * error (required)
                        * values are one of
                            * invalid_request
                            * unauthorized_client
                            * access_denied
                            * invalid_scope
                            * unsupported_response_type
                            * server_error
                            * termporarily_unavailable
                    * error_description (optional)
                        * human readable description of error
                    * error_uri (optional)
                        * URI for human to find out more about error
                    * state (required if client gave server a state parameter)
            1. client-app makes a request to the authorization server token endpoint
                * client-app authenticates _itself_ with the authorization server during this step
                    * usually this will be by sending a "secret" via the HTTP Authorization header
                * parameters
                    * grant_type=authorization_code (required)
                    * code (required)
                        * the authorization code from the previous steps
                    * redirect_uri (required)
                        * this redirect uri MUST match the one sent by the client as part of trying to get the authorization grant in the previous steps
                    * client_id (required)
                        * part of how the client authenticates itself with the authorization server
                * example
                    ```
                    POST /token HTTP/1.1
                    Host: server.example.com
                    Authorization: Basic czZCaGRSa3F0MzpnWDFmQmF0M2JW
                    Content-Type: application/x-www-form-urlencoded

                    grant_type=authorization_code&code=SplxlOBeZQQYbYS6WxSbIA
                    &redirect_uri=https%3A%2F%2Fclient%2Eexample%2Ecom%2Fcb
                    ```
            1. server does some checks
                * is client authentication ok?
                * does the redirect_uri match the one stored in the previous steps
                * is the authorization code valid?
                    * has it expired?
                    * has it been used before?
            1. authorization server replies with an access token (and optional refresh token) to the client
                * example
                ```
                HTTP/1.1 200 OK
                Content-Type: application/json;charset=UTF-8
                Cache-Control: no-store
                Pragma: no-cache

                {
                "access_token":"2YotnFZFEjr1zCsicMWpAA",
                "token_type":"example",
                "expires_in":3600,
                "refresh_token":"tGzv3JOkF0XG5Qx2TlKWIA",
                "example_parameter":"example_value"
                }
                ```
        * pros/cons of this grant type
            * ++ the access token is sent directly to the client, not through
              the user's "user agent"
            * ++ the client-app can be authenticated with a secret token agreed
              ahead of time and known only to it and the auth-server (Facebook
              does this)
            * -- quite a few round trips required to get the protected resource
              (performance penalty)
    2. implicit grant type
        * a simplified flow
        * optimized for in-browser client-apps e.g. JS apps
        * access token is issued directly - no authorization code step
        * does not support refresh tokens
        * flow
            1. client registers itself with the authorization server
                * it gets a client id but NOT a client secret (the browser could not keep it secure so no secret is used)
                * this may happen a long time before the other steps
            1. client-app sends user to authorization server
                * parameters
                    * response_type=token (required)
                    * client_id (required)
                    * redirect_uri (optional)
                    * scope (optional)
                    * state (recommended)
                * example
                  ```
                  GET /authorize?response_type=token&client_id=s6BhdRkqt3&state=xyz
                      &redirect_uri=https%3A%2F%2Fclient%2Eexample%2Ecom%2Fcb HTTP/1.1
                  Host: server.example.com
                  ```
            1. authorization server validates request and checks that the redirect_uri is actually registered by the given client_id
            1. user authenticates themselves with authorization server
            1. user chooses what kind of access to give the client-app to their protected resources
            1. auth-server creates an "access token" that allows client-app to access protected resource
            1. auth-server redirects responds to the request with a HTTP 302
               and a `Location` header that is the client provided redirect_uri
               with some parameters appended.
                * parameters:
                    * acces_token (required)
                    * token_type (required)
                    * expires_in (recommended)
                    * scope (optional)
                    * state (required)
                * the authoriztion server MUST NOT issue a refresh token
                * example
                ```
                HTTP/1.1 302 Found
                Location: http://example.com/cb#access_token=2YotnFZFEjr1zCsicMWpAA
                        &state=xyz&token_type=example&expires_in=3600
                ```
                * An error is returned if somethign goes wrong - see section 4.2.2.1 for details
                    * error example
                    ```
                        HTTP/1.1 302 Found
                        Location: https://client.example.com/cb#error=access_denied&state=xyz
                    ```
            1. The client makes a request to the `Location` HTTP header it got from the previous step
            1. This loads a new HTML document with both the access_token and JS that can make use of it
            1. client (JS app) uses the access token to access protected resource on a server
        * pros/cons
            * -- no authentication of the client by the authorization server
                * browser cannot be trusted with a client secret
                * in some cases this may be possible by inspecting the redirect URI
                  used to get from client-app to auth-server but this URI is
                  exposed to the user and their user-agent (browser)
            * -- the access token is embedded in the redirection URI so can be visible to
                1. resourse owner
                2. other applications on the same device
            * ++ less round-trips require to obtain access
            * ++ useful for JS apps that need access to a resource for a short time (few hours)
    3. resource owner password credentials grant type
        * user's credentials are entered into the client and used to obtain an access token
        * flow
            1. client-app shows UI to allow user to enter their credentials
            1. client-app sends credeitals to auth-server which sends back
            access token
            1. client (ideally) discards user credentials and uses access token
            from then on
                * ??? client-app may also get a "refresh token" to allow it to
                  get new access tokens ???
                    * I presum the win here is that the refresh token is safer
                      to store in client-app than the users creds
        * pros/cons
            * -- only useful is there is a high level of trust between
              client-app and auth-server
            * -- the client-app can see and store the users credentials
            * ++ client-app only needs the users credentials to get the access
              token so it doesn't have to store them or put them on the wire
              for each request
    4. client credentials
        * some for of client-app authentication (e.g. username+password or
          secret key) is used as the authorization grant
        * used when the authorization scope is limited to things the client-app
          already has
        * used when the client-app is also the resource-owner i.e. it is acting
          on its own behalf
        * in a way the client-app is asking the auth server to allow it to
          access things it already has
        * ??? not sure of the use cases here ???
* more grant types can be defined via an extensibility mechanism
    * ??? are there others actually in use ???

### Access token

* a string representing an authorization issued to the client-app
    * has scopes
    * has a duration
    * resource owner who granted it
* provides an abstraction layer over the authorization required to get access to the protected resource
* is usually opaque to the client (but not always???)
* two forms
    * it may be a string that can be used by the resource server to look up the details of the authorization
    * it may contain the authorization details embedded in the string and signed
* details are not defined in the main RFC
    * RFC6750 has more details on the format of access tokens

* token analogy
    * access token is like a session
    * refresh token is like a password

Tokens can be
* passed by value
    * all the information required is embedded in the token
* passed by reference
    * a reference/pointer to the acutal data
    * for these, the resource server has to make a call to the authorization server to "dereference it" and check that the user is authorized

Token profiles
* bearer token
    * a bit like cash
    * it has the issuer on there, the merchant you are transacting with is happy to take it because they trust the issuer
* holder of key
    * a bit like a credit card
    * ties the holder to the token
    * merchant has to ask for identification as the token is presented
* OAuth spec does not say waht kind of tokens are exchangted
* there are many pssible tokens you can use with OAuth
    * WS-Security
    * SAML
    * JWT
    * Custom
        * Home-grown
        * Oracle Access Manager
        * SiteMinder
    * etc.
* this token flexibility simplifies integration for enterprises


### Json identiy protocol suite

* Suite of JSON based identity protocols
    * Tokens (JWT)
        * pronounced "jot"
        * lightweight, designed to be passed in HTTP headers and query strings
            * more suitable for IoT and mobile devices
        * Akin to SAML tokens
            * less expressive
            * less security optionds
            * more compact
            * encoded w. JSON not XML
    * Keys (JWK)
    * Algorithms (JWA)
    * Encrpytion (JWE)
    * Signatures (JWS)
* The bearer token spec explains how to use with OAuth
* these are being defined in the IETF

"nonce" stands for "not more than once"
### Refresh token

* a string
* used to obtain access tokens
* issued to the client by the authorization server at the same time the access token is issue
    * issued as the response to the authorization grant sent by the client
* refresh tokens are only sent to the authorization server - they are never sent to the resource server
* the authorization server may send a new refresh token and access token when it receieves a refresh token
    * allows the auth server to extend the lifetime of the authorization provided it is being used
* uses
    * obtain a new access token when the current one becomes invalid or expires
    * obtain new access tokens with identical or narrower scope (either in resources available or duration)
* usually opaque to the client


OAuth used HTTP redirects a lot
    * authorization server redirects the resource-owners user-agent to a different destination
    * client-app redirects the resource-owners user-agent to a different destination

The requirement for profiles

* OAuth 2 (the spec) on its own is a framework not a way to build interoperable security systems
* You must use the defined profiles to build real systems


### Client registration process

* Before the protocol it is assume that the client has already registered with the authorization server
* _How_ registation happens is mostly outside of OAuth but there are some bits
  it needs in place:
    1. client type (confidential|public) - the client should say whether it can secure its credentials
        * "secure" is from the POV of the authorization server i.e. what level
          of compromised client credentials it is ok being exposed to.
    2. client redirection URIs
    3. Any other info required by the authorization server e.g.
        * app anme
        * website
        * description
        * logo
* Focusing on the redirection endpoint:
    * When does a client need to register a redirect URI?
        * registration of a redirection endpoint is _required_ for
            * public clients
            * confidential clients using the implicit grant type
        * registration of a redirection endpoint is _optional_ for
            * confidential clients using the authorization-code grant type
            * even then the spec recommends all authorization servers require registration
    * It is allowed for the client to register multiple redirection endpoints
        * ??? usage example ???
* examples
    * creating a Facebook app that allows your app (the "client") to access
      user data protected by facebooks resource servers.
* when a client is registered it gets a "client identifier" from the authorization server
    * this is a string (length decided and documented by server)
    * it is unique to the authorization server
    * it is NOT a secret - it is exposed to the resource owner
    * the authorization server should not be silly enough to use just the client identifier to authenticate the client

The spec does not ban unregistered clients but also doesn't define how things
might work with them except to say that "a security review is required" ...
    * TL;DR - clients should be registered

### Client type and client profile

Client type

There are two kinds of client-type:

1. confidential
    * a client capable of secure client authentication e.g.
        * capable of securing the stored client credentials
        * capable of doing client auth in a way that doesn't compromise credentials
1. public
    * a client not capable of securing their credentials e.g. facebook SDK in native mobile app

* ??? I _think_ that large public APIs (e.g. Facebook, Github) assume the client-type is "public" ???
* ??? I _think_ it is up to the implementor to decide whether they consider a particlar client secure enough to be "confidential" ???

The spec was designed for three client "profiles"

1. web application
    * client is a web server app
    * resource owner interfaces it via HTML
    * a confidential client
        * client credentials and access tokens are stored on server and not exposed to the resource owner
1. user agent based application
    * JS app
    * a public client
        * it runs in browser and credentials and access tokens sent to it are potentially exposed to the resource user
1. native application
    * a public client
        * protocol data and credentials are accessible to the resource owner
        * assumed that any client credentials can be extracted
        * dynamically issued credentials such as access tokens, refresh tokens can be somewhat secured
            * depends on the platform
            * maybe secured from other apps on platform or other servers the client interacts with

Ways a *registered* client may authenticate with an authorization server (unregistered clients are undefined in the spec)

* If the client is confidential i.e. can be trusted with client authentication credentials
    * any suitable HTTP authentication method that the authorization server deems suitable is ok by the spec
    * HTTP basic auth is probably the most common - the spec goes into detail about how the client id and secret (password) should be encoded.
    * examples
        1. password
            * if the client has a password the then authorization server MUST allow it to submit that password via HTTP Basic auth
            * the spec says how the `client_id` (client identifier) and `client_secret` (password) are encoded for HTTP basic auth
                * see section 2.3.1
            * authorization server must require TLS for endpoints sending client authentication info
            * authorization server should protect against brute force attacks on that endpoint
                * ??? how does doorkeeper do this ???
        2. public/private keypair
            * mentioned but no details given
            * TODO: find out more, is this used, if so where?
* If the client is public
    * section 2 has nothing to say about this case ...


### Authorization server endpoints

The authorization process uses up to two authorization server endpoints and one client endpoint:

* Authorization server
    1. Authorization endpoint
        * used by the client to obtain authorization from the resource owner via user-agent redirection
        * used in grant type flows:
            * authorization code
            * implicit
    2. Token endpoint
        * the client must have authenticated itself with the authorization server _before_ it hits the token endpoint
        * used by the client to exchange an authorization grant or refresh token for an access token
            * "typically wtih client authentication" ????
                * does that mean the client has to authenticate itself with the authorization server before hitting this endpoint????
        * used for every grant type except _implicit_.
        * client use use HTTP POST method to get access tokens
        * spec does not specify a way for the client to figure out the URI of the token endpoint - it is usually hardcoded
* Client
    1. Redirection endpoint
        * Used by the authorization server to return responses containaing authorization credentaisl to the client via th resource owner user-agent

* Not every authorization grant type uses both endpoints
* Spec allows extensible grant types to define their own endpoints

Both authorisation server endpoints (authorization, token) allow the client to provide a "scope" parameter

* If the client provides a scope parameter, the server will return the scope in the respone to indicate to the client which scopes it actually got
* a "scope" is
    * a space delimited set of strings
    * order does not matter
    * defines the access the client wants
* if the client does not send a scope parameter the server should fall back to a default scope

I _think_ scopes are a way of the client requiesting access to a subset of the resources it _could_ get.
??? can scopes be a superset of the default scope ???



## How multiple redirect_uri work

* Most OAuth2 authorization services allow you to specify multiple `redirect_uri` values when you register your client application
* These are the list of "legal" redirect URIs that the server should accept for your client application
* The spec (3.1.2.2) says that
    * the server should force you to register the full URI `scheme://authority/path?query` so you can only vary the `state` query parameter.
    * but at the very least it should force you to register `scheme://authority/path`

The `redirect_uri` is sent from client to authorization server twice during the authorization code flow:

1. `redirect_uri` 1 == the client sends to authorization server when trying to get the authorization code
2. `redirect_uri` 2 == the client sends to authorization server when trying to exchange authorization code for access token

not all providers perform exact matches of the redirect URI, although the spec
requires it. To counter this the spec tries to add a layer of security by
having the server check that the second redirect_uri it gets matches the first and also that they match one of the registered URIs

http://security.stackexchange.com/questions/44214/what-is-the-purpose-of-oauth-2-0-redirect-uri-checking


# Token profiles (Bearer vs Holder of Key)

from http://nordicapis.com/api-security-oauth-openid-connect-depth/

There are two token profiles

1. Bearer tokens
2. Holder of Key (HoK) tokens (still in draft I think)

You can think of bearer tokens like cash. If you find a dollar bill on the ground and present it at a shop, the merchant will happily accept it. She looks at the issuer of the bill, and trusts that authority. The saleswomen doesn’t care that you found it somewhere. Bearer tokens are the same. The API gets the bearer token and accepts the contents of the token because it trusts the issuer (the OAuth server). The API does not know if the client presenting the token really is the one who originally obtained it. This may or may not be a bad thing. Bearer tokens are helpful in some cases, but risky in others. Where some sort of proof that the client is the one to who the token was issued for, HoK tokens should be used.

HoK tokens are like a credit card. If you find my credit card on the street and try to use it at a shop, the merchant will (hopefully) ask for some form of ID or a PIN that unlocks the card. This extra credential assures the merchant that the one presenting the credit card is the one to whom it was issued. If your API requires this sort of proof, you will need HoK key tokens. This profile is still a draft, but you should follow this before doing your own thing.





OAuth 2 has 7 flows (I don't know how flows map onto what I have read in spec yet)

1. original OAuth 1.0 flow
    * ??? I thought 2 was not compatible with 1 ???
2. User-Agent flow
    * for clients within a web browser
3. Web Server Flow
    * for clients that are part of a web server (accessible via HTTP)
    * a simplified version of the OAuth 1 flow
4. Device Flow
    * clients on limited devices
    * end-user has access to a browser on another computer or device
5. Username and Password Flow
    * use if user trusts the client to handle its credentials but the client should still not store username & password
    * use only if there is a lot trust between user and client
6. Client Credentials Flow
    * supports the "8-legged scenario"
    * client uses its credentials to obtain an access token
7. Assertion Flow
    * client presents an assertion e.g. "SAML assertion" to the authorization server in exchange for an access token

### controversy around 2

* From the lead author: http://hueniverse.com/2012/07/26/OAuth-2-0-and-the-road-to-hell/

> When compared with OAuth 1.0, the 2.0 specification is more complex, less
> interoperable, less useful, more incomplete, and most importantly, less
> secure.

> It is this extensibility and required flexibility that destroyed the protocol.
> With very little effort, pretty much anything can be called OAuth 2.0
> compliant.

> What 2.0 offers is a blueprint for an authorization protocol. As defined, it
> is largely useless and must be used with profiles to become a working
> solution – and that is the enterprise way.

### APIs using OAuth 2.0

* Facebook
* Github
    * Github is a good example of a popular OAuth 2.0 API
    * https://api.github.com/authorizations
* ... many others ...

## OAuth in Ruby

* In ruby there are `OAuth` an `OAuth2` gems but there are usually service
  specific gems too.
* Use `omniauth` gem if you just wnat to authenticate your users against some
  other service.
    * Use it to let your users auth against something else and then become users on your site.
    * If you want to make use of that other site's API properly, use a gem specific to it
    * Lots of community strategies available
    * A strategy is a rack middleware so will work with anything that uses rack

## Doorkeeper

In doorkeeper will look for the access token in the request object as follows

1. The HTTP_AUTHORIZATION header
2. params.access_token
3. params.bearer_token

so you can either use the `Authorization` header or encode the token in the body of the request

Authorization header is probably easiest

Note that the correct way to encode an access token as the HTTP Basic Auth header is

```
Authorization: Bearer <access token>
Authorization: Bearer 2dc7fedf1c59eeb78345e4bfaf45ea6552038ebcd9dc94d47b25b39780c20fdd
```
