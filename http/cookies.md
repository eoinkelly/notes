# Cookies

https://developer.mozilla.org/en-US/docs/Web/HTTP/Cookies


1. Name
2. Value
3. Domain
    * hosts which the browser should send this cookie back to
        * subdomains are included e.g. `Domain=mozilla.org` will include this cookie on `foo.mozilla.org` and `bar.mozilla.org`
    * defaults to the host of the current document if missing **excluding subdomains**

4. Path
    * browser will only send the cookie to a URL which contains this path
5. Expires
6. Size
7. HTTP flag
8. SameSite
    * not supported by all browsers
    * mitigated CSRF somewhat
9. Secure
    * the cookie should only be sent with HTTPS requests
    * if this flag is set then the cookie cannot be accessed by JS


## Cookie expiry

A cookie is automatically a "session cookie" i.e. will be deleted when the browser tab is closed unless you specify an Expires or Max-Age directive in the cookie

* Expires sets a date & time that the cookie should expire
* Max-Age sets a maxiumum age of the cookie in (date and time are relative to the client not the server)

> web browsers may use session restoring, which makes most session cookies permanent, as if the browser was never closed

Session restore

* Browsers will automagically keep your "session" going  in some cases
    * e.g. Firefox
        * browser crash
        * addon install
        * user explicitly asks for a session restore

## Cookie scope

* Domain and Path together decide the scope of the cookie

## Setting cookies

Set a cookie by setting a `Set-Cookie` header in the response
The browser will return it back to you in the `Cookie` header


```
Set-Cookie: <cookie-name>=<cookie-value>

Set-Cookie: _healthpromotion_session=U2tOUENhNFVTdTRwcmtKNkVCQnIvckZmSHd2anYycjZQaWtJUnliN3dPcEZmTlY4ZVIwM2tKWU9xdEZlRXdGcVhlc09mRmtNM0w4RGpUaU9sMjM0UVhvY2QvUXh5S3FnQTRYSWJaQnpZamhZRjlYV2RtZFc3RW5HUmpxZEhabXA5aElWKzNXRVl6R1BRUGtuZWN3c3dRPT0tLXNYVnk3b2ljWkJlSkV1a09BY3AxbUE9PQ%3D%3D--867fbc5759582366b914f00b9c1274e030999499; path=/; HttpOnly

Set-Cookie: yummy_cookie=choco; tasty_cookie=strawberry
Set-Cookie: id=a3fWa; Expires=Wed, 21 Oct 2015 07:28:00 GMT; Secure; HttpOnly
```

