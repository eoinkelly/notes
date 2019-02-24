# Devise Token authenticatable

This token can be given via

1. a query string or
2. HTTP Basic Authentication.

Strategy for signing in a user, based on a authenticatable token. This works
for both params and http. For the former, all you need to do is to pass the
    params in the URL:

http://myapp.example.com/?user_token=SECRET

For headers, you can use basic authentication passing the token as username and
blank password. Since some clients may require a password, you can pass “X” as
password and it will simply be ignored.


## Flow


```
when the user hits any rails app endpoint it gets back an `_appname_session` via Set-Cookie
the browser sends that cookie back with every request


so waht does devise/warden do

GET /users/sign_in
server returns HTML Form with an embedded CSRF field
POST /users
server verifies the username & password
returns reply with a Cookie representing the session that has been created on the server
```

## Rails session cookies vs JWT (in the context of a rails app)

* Rails session cookie
    * ++ work out of the box
* JWT tokens
    * ++ less vulnerable to CSRF attacks because you have to explicitly add the Authorization header with the token (rather than the browser automatically attaching the Cookie to every request)
        * this is probably more convenient in a rails app?
    * ++ are easier to use if your client is not a web browser e.g. a native mobile app
        * QUESTION: How much easier? It seems like you are attaching a HTTP header in either case?
    * ++ are much easier if you auth against one domain and want to access resources on a different domain
        * this explains why things like Auth0 are fans of them - they are required for their business model :-)
    * ++ has a built-in expiration timestamp
        * you are relying on the browser to expire session cookies - JWT can let server control it
        * caveat: the server implementation has to actually check this (some don't)

