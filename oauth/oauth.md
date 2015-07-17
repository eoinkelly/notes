# Oauth

* allows you to grant 3rd party access to your web resources without sharing their passwords
* you cannot run Oauth without SSL and expect it to be secure
    * you have to send the "token" and the "secret" to the client somehow


Ruby gems

* In ruby there are `oauth` an `oauth2` gems but there are usually service
  specific gems too.
* Use `omniauth` gem if you just wnat to authenticate your users against some
  other service.

## Sources

* http://oauth.net/
* oauth 1 guide: http://hueniverse.com/oauth/guide/

## Version 1

oauth 1 = RFC 5849.
* largely based on
    * Flicker API Auth
    * Google AuthSub
* -- requires crpyto capability on client-side
* -- did not scale particularly well
    * -- requires state management across setps
    * -- issues long lasting credentials (less secure, harder to sync across data centers)
* -- required endpionts to have access to both the "client credentials" and
  "token credentials" which prevents enterprises from having a centralized
  authorization server to issue credentials and a separate server for API calls

Methods fore obtaining access tokens are called "flows"
* 1.0 has 3 flows:
    * web based apps
    * desktop clients
    * mobile/limited devices
    but they get merged into one which in practice only worked well for web based apps

## Version 2

* oauth 2 is not backwards compat iwth 1
* 2 is more of a framework than a protocol

* does not require clients to have cryptography
* 2.0 signatures are much less complicated
* 2.0 access tokens are "short lived"
    * server can issue short lived "access token" and a long-lived "refresh token"
    * the client can use the "refresh token" to obtain a new access token
      without involving the user again but still keep the tokens limited
* has better support for non browser based applications
    * 1.0 required apps to open a browser to complete auth
* separates roles
    * the "authorization server" (thing that issues tokens) and the resource server handling API calls are two distinct roles
        * in many cases both roles are on the same machine but they don't have to be (allows scaling)

* has 7 flows

1. original oauth 1.0 flow
1. User-Agent flow
    * for clients within a web browser
1. Web Server Flow
    * for clients that are part of a web server (accessible via HTTP)
    * a simplified version of the OAuth1.0 flow
1. Device Flow
    * clients on limited devices
    * end-user has access to a browser on another computer or device
1. Username and Password Flow
    *use if user trusts the client to handle its credentials but the client should still not store username & password
    * use only if there is  a lot trust between user and client
1. Client Credentials Flow
    * supports the "2-legged scenario"
    * client uses its credentials to obtain an access token
1. Assertion Flow
    * client presents an assertion e.g. "SAML assertion" to the authorization server in exchange for an access token

### controversy around 2

* From the lead author: http://hueniverse.com/2012/07/26/oauth-2-0-and-the-road-to-hell/

> When compared with OAuth 1.0, the 2.0 specification is more complex, less
> interoperable, less useful, more incomplete, and most importantly, less
> secure.

> It is this extensibility and required flexibility that destroyed the protocol.
> With very little effort, pretty much anything can be called OAuth 2.0
> compliant.

> What 2.0 offers is a blueprint for an authorization protocol. As defined, it
> is largely useless and must be profiles into a working solution – and that is
> the enterprise way.

# Omniauth gem

* Use it to let your users auth against something else and then become users on your site.
* If you want to make use of that other site's API properly, use a gem specific to it
* Lots of community strategies available
* A strategy is a rack middleware so will work with anything that uses rack
