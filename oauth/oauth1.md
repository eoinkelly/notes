# Oauth

* In ruby there are `oauth` an `oauth2` gems but there are usually service
  specific gems too.
* Use `omniauth` gem if you just wnat to authenticate your users against some
  other service.

## Sources

* http://oauth.net/
* oauth 1 guide: http://hueniverse.com/oauth/guide/

## Version 1
oauth 1 = RFC 5849.


## Version 2

* oauth 2 is not backwards compat iwth 1
* 2 is more of a framework than a protocol

### controversy around 2

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
