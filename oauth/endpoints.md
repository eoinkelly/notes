# OAuth2 endpoints

There are three endpoints involved in OAuth2

* two on the auth server
    1. authorization endpoint (`/oauth/authorize` in doorkeeper)
        * MUST use TLS/SSL
        * MUST support GET
        * MAY support POST
    1. token endpoint (`/oauth/token` in doorkeeper)
* one on the client
    1. redirect endpoint
        * MUST be an absolute URL
        * MAY have a query string
        * MUST NOT have a fragment

Different flows use different endoints

* Authorizaiton code
    * uses all three endpoints
* Implicit
    * uses all three endpoints
* Resource owner password credentials
    * uses only token endpoint on auth server
* Client credentials
    * uses only token endpoint on auth server

## endpoints provided by doorkeeper but not in spec

* `/oauth/token/info`
    * can be used by a token holder to verify token e.g. resource server can verify tokens here
* `/oauth/revoke`
    * can be used to revoke a token
