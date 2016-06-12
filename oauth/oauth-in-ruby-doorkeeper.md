## OAuth in Ruby

* In ruby there are `OAuth` an `OAuth2` gems but there are usually service
  specific gems too.
* Use `omniauth` gem if you just wnat to authenticate your users against some
  other service.
    * Use it to let your users auth against something else and then become users on your site.
    * If you want to make use of that other site's API properly, use a gem specific to it
    * Lots of community strategies available
    * A strategy is a rack middleware so will work with anything that uses rack
* Use `doorkeeper` if you need an OAuth2 authorization server implementation

# OAuth2 authorization server gem: doorkeeper

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

Doorkeeper and token validation

* can do token validation across the wire though the `token_info` controller but it isn't documented  - see https://github.com/doorkeeper-gem/doorkeeper/issues/365
