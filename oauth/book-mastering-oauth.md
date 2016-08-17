# Mastering OAuth 2.0

## Chapter 1

* OAuth 2.0. is not backwards compatible with 1.0

* Definitions
    * Authentication = validating whether a person or system is who they say the are
    * Authorization = the _process_ of deciding what actions a particular
      user/system is allowed to perform.

Use cases for OAuth2

1. Delegate authority
    * allow service A to access resources service B on behalf of a user
    * protected resource: whatever resources service B is protecting for that user
1. Federated identity
    * Allow a user to login to service A with an account from service B
    * protected resource: user account details on service A
    * OAuth2 can't do this alone - it needs OpenID on top to provide authentication

These are the same thing! A *client* is accessing a *protected resource* on
behalf of a *user* in both cases.

* **OAuth2 is an authorization protocol not an authentication protcol!**
    * the OAuth2 spec never says **how** a user proves their identity to the auth server
* From the spec
    > The way in which the authorization server authenticates the resource owner
    > (e.g., username and password login, session cookies) is beyond the scope of
    > this specification.

OpenID can provide an authentication layer on top of OAuth2

OAuth2 solves the problem of

* I want to give app A "limited" access to my data on service B
    * limited = time limited and scope limited
    * i don't want to give my service B creds to app A
    * I definitely don't want app A to store my service B creds
    * I want to be able to revoke access

## Chapter 2

* OAuth2 has 4 workflows overall but 2 main ones:
    * each main workflow is designed for a particular kind of **client**

1. Trusted client (confidential client)
    *  can securely *store* secrets
    * called a "confidential client" in spec
    * example: a server
    * The "authorization code" grant is designed for this client
2. Untrusted client (public client)
    * cannot securely *store* secrets
    * called a "public client" in spec
    * example:
        * JS app that runs in browser
        * native mobile app
            * even if it uses the native secure storage APIs of the device it
              probably shouldn't be trusted.
            * sometimes these are considered trusted (depends on use-case)
    * The "implicit" grant is designed for this client

In determining the difference between trusted and untrusted client, usually it
comes down to the *storage* of secrets (SSL means secure transmission is
easy for all clients to do)

NOTE: The security of OAuth2 completely depends on being over SSL

Terminology: "grant"

* grant can mean
    * A workflow
    * A JSON response

Available Workflows

1. Authorization code grant
    * also known as "server side workflow" or "trusted client workflow"
2. Implicit grant
    * also known as "client side workflow" or "untrusted client workflow"
3. Resource owner password credential grant
    * less common
    * is available to migrate users using HTTP basic auth
        * => it works very much like HTTP basic auth and provides similar security trade-offs
    * has the following benefits over HTTP basic auth:
        * the client can (hopefully) not store the resource owner's username
          and password once it has the token (and optional refresh token)
        * the client still has to store these tokens but this is still a bit
          better than directly storing the username and password of the
          resource owner.
4. Client credentials grant
    * less common
    * the client app is getting access to the resource on **its own** behalf
      not on behalf of a user.
        * Only works with trusted clients
    * is similar in flow to HTTP Basic Auth
    * example uses
        * app to app communication
        * a client-app wants to get some info about itself from the auth server e.g. `/client-info`
            * this info is treated as a resource protected by the auth server
              but accessed on behalf of the client itself.

## Untrusted client using implicit grant workflow

1. You ask GoodApp to show you a list of your contacts on Facebook
2. GoodApp sends you to a Facebook.com URL. The URL contains metadata to let Facebook process it e.g.
    * the client_id of GoodApp (so Facebook knows that it is GoodApp wants access to your stuff)
    * scopes to tell Facebook *what* stuff you want from the user
    * An **optional**  "redirect URI" (tells Facebook which URL in GoodApp to send the result of the access request to)
3. Facebook asks the user if they are ok with GoodApp having access to the requested stuff (we assume they say yes)
4. Facebook sends a key (access token) to the GoodApp redirect URL
5. GoodApp sends a request to Facebook for the contacts passing the key it received
6. Facebook validates the key
    * *how* this is done is not specified by OAuth2
7. Facebook sends back the list of contacts


Features of this flow

* was explicitly designed for clients which can't store credentials securely
* ++ backend server does not need to know how to do OAuth2.
    * You still need a backend server to serve your app initially and also when
      Facebook redirects to it but the backend server doesn't need to know about
      OAuth.
* -- less secure because client is considered untrusted
* client cannot be trusted to store credentials or tokens so
    * no refresh token available
    * the access tokens (keys) have a short expiry

Implementation tips

* Try to limit access to read-only for untrusted clients

### Trusted client using authorization grant workflow

1. You ask GoodApp to show you a list of your contacts on Facebook
2. GoodApp sends you to a Facebook.com URL. The URL contains metadata to let Facebook process it e.g.
    * the client_id of GoodApp (so Facebook knows that it is GoodApp wants access to your stuff)
    * scopes to tell Facebook *what* stuff you want from the user
    * A "redirect URL" (tells Facebook which URL in GoodApp to send the result of the access request to)
3. Facebook asks the user if they are ok with GoodApp having access to the requested stuff (we assume they say yes)
4. Facebook sends a key (access token) to the GoodApp redirect URL (which is on the GoodApp server)
5. GoodApp server sends a request to Facebook for the contacts passing the key it received
6. Facebook validates the key
    * *how* this is done is not specified by OAuth2
7. Facebook sends back the list of contacts to the GoodApp server and it in turn presents you with some appropriate UI based on that

Consequences

* GoodApp server now has in its posession
    1. A now-useless authorization grant code (they are single use and expire quickly when unused)
    2. An access token which allows it to access exactly the set of data you permitted and has a set expiry
    3. Potentially a refresh token that GoodApp server can use to renew its access token

## Aside: OAuth2 authorization codes are "consumable"

Authorization codes should be considered "consumable" i.e. they are single use and expire quickly if unused

## Aside: OAuth2 access tokens are bearer tokens

* bearer token is a category of token that functions like a physical key
* whoever has the token can access the resource - no questions asked

## Chapter 3


APPENDIX NOTES ARE COMPLETE

# Comparing "resource owner password credentials" and "client credentials" grant types

Both _resource owner password credentials_ grant and the _client credentials_
grant work **very** similarly.

The only difference between them is that _resource owner password credentials_
grant sends a username and password as part of the request.

## Appendix A: Resource owner password credentials grant

* less secure because the user enters their password directly into the client app
* useful for migrating existing clients who use HTTP basic/digest authentication
* it is slightly more secure than those schemes because the password is only on
  the wire once and is exchanged for a token. Tokens are better because:
    1. tokens expire
    2. tokens can be scope limited

Flow

1. Client requests access token from the authorization server **token** endpoint (note: not authorization endpoint)
    * The request includes parameters encoded in the `application/x-www-form-urlencoded` format
        * `grant_type=password` (required)
        * `scope` (optional)
            * a space delmited, case sensitive set of strings that represent the requested access level
    * The client authenticates itself to the server in this request by "some means" i.e. OAuth2 has nothing to say about how this happens. Common choices are
        1. HTTP Basic auth
        1. HTTP digest auth
        1. Public/private key auth
1. Authorization server responds with access token JSON
    * response can also contain a refresh token, expires_in, scope similar to the token endpoint response during other grant types

## Appendix B: Client credentials grant

* this grant type focuses on getting an access token on behalf of an application not a user

Flow

1. Client requests access token from the authorization server **token** endpoint (note: not authorization endpoint)
    * The request includes parameters encoded in the `application/x-www-form-urlencoded` format
        * `grant_type=client_credentials` (required)
        * `username` (required)
        * `password` (required)
        * `scope` (optional)
            * a space delmited, case sensitive set of strings that represent the requested access level
    * The client authenticates itself to the server in this request by "some means" i.e. OAuth2 has nothing to say about how this happens. Common choices are
        1. HTTP Basic auth
        1. HTTP digest auth
        1. Public/private key auth
1. Authorization server responds with access token JSON
    * response can also contain a refresh token, expires_in, scope similar to the token endpoint response during other grant types

This is basically an "oauth flavour" to standard keyed API access

What are the advantages of this over using an API auth scheme that doesn't have the "oauth topping"

* ++ this has the "OAuth" brand
* ++ there is some documentation and shared understanding of
    * endpoint to use
    * request format
    * success response format
    * error response format
    * if/how to refresh access
* ++ this has support for scopes so an app can self-limit its own access
* ++ there is a better chance that the client developer will have access to a lib in their chosen language that supports this flow

Examples of APIs that use _Client credentials grant_

* Vimeo
    * uses this flow when you access publically available content
* MS Azure
    * allows you to use it to communicate between apps <https://msdn.microsoft.com/en-nz/library/azure/dn645543.aspx>
