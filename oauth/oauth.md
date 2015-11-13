# OAuth

* allows you to grant 3rd party access to your web resources without sharing their passwords
* is an _authorization_ protocol not an _authentication_ protocol
* NOTE: you cannot run OAuth without SSL and expect it to be secure because you
  have to send the "token" and the "secret" to the client somehow

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
    1. authorization code
        * is an "intermediate credential"
        * flow
            1. client-app sends user to auth server
            1. user authenticates themselves with auth server
            1. user chooses what kind of access to give the client-app to their
            protected resources
            1. auth server creates an "authorization code" that represents this
            granting of access
            1. auth server redirects user back to client-app passing along the
            "authorization code"
            1. client-app uses the authorization code to get an acess token
            which can then be used to get the protected resource
        * pros/cons
            * ++ the access token is sent directly to the client, not through
              the user's "user agent"
            * ++ the client-app can be authenticated with a secret token agreed
              ahead of time and known only to it and the auth-server (Facebook
              does this)
            * -- quite a few round trips required to get the protected resource
              (performance penalty)
    2. implicit
        * a simplified flow
        * optimized for in-browser client-apps e.g. JS apps
        * access token is issued directly - no authorization code step
        * flow
            1. client-app sends user to auth-server
            1. user authenticates themselves with auth-server
            1. user chooses what kind of access to give the client-app to their
            protected resources
            1. auth-server creates an "access token" that allows client-app to
            access protected resource
            1. auth-server redirects back to client-app passing along the
            access token
            1. client-app uses the access token to access the protected
            resource
        * the auth-server does NOT authenticate the client-app in this flow
        * in some cases this may be possible by inspecting the redirect URI
          used to get from client-app to auth-server but
            * -- this URI is exposed to the user and their user-agent (browser)
        * pros/cons
            * -- no solid way for auth-server to verify identity of client-app
            * ++ less round-trips require to obtain access
    3. resource owner password credentials
        * user's credentials are entered into the client-app and used to obtain
          an access token
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

Access token

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


Refresh token

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

UP TO 3.1.2.2.

OAuth 2 has 7 flows

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

```ruby

require 'httparty'

token = "1c4b40471046fdb5d0ff81570c7564fe244940c2"

user = HTTParty.get "https://api.github.com/user",
        :headers => {
                        "Authorization" => "token #{token}",
                        "User-Agent" => "codecademy"
                    }

puts "Hi, my username is #{user["login"]}"

```
