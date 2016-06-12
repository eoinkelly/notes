# Token validation

* The process of a _resource server_ validating the access token it received is refered to as
    * token validation
    * authorization check
* If the "resource server" role and "authorization server" role have access to
  the same DB then token validation is straightforward and can be done
  in-memory.
* If the "resource server" role and "authorization server" role are not the
  same server (or at least don't have access to the same DB) then token
  validation needs to happen over the wire
* original OAuth spec does not specify how to do this
* A process is outlined in RFC 7662 <https://tools.ietf.org/html/rfc7662> in Oct 2015
    * NOTE: date is recent so implemenations will take time
* There are some existing implementations - <http://stackoverflow.com/a/20010178/356025> for a good overview
    * Google: <https://developers.google.com/accounts/docs/OAuth2UserAgent#validatetoken>
    * Github: <http://developer.github.com/v3/oauth/#check-an-authorization>
    * Amazon: <https://images-na.ssl-images-amazon.com/images/G/01/lwa/dev/docs/website-developer-guide._TTH_.pdf> (page 21 of that pdf)

> The OAuth 2.0 spec doesn't clearly define the interaction between a Resource
> Server (RS) and Authorization Server (AS) for access token (AT) validation. It
> really depends on the AS's token format/strategy - some tokens are
> self-contained (like JSON Web Tokens) while others may be similar to a session
> cookie in that they just reference information back at the AS in memory.
>
> http://stackoverflow.com/questions/12296017/how-to-validate-an-oauth-2-0-access-token-for-a-resource-server
