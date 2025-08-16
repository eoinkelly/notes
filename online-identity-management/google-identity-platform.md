# Google Identity Platform

From least-dev to most-dev

1. Firebase (takes care of all of your auth, not just authing against google
   accounts)
1. "Google Sign-in"
    - is a JS library (part of the google JS API)
    - manages the OAuth2 flow and token lifecycle i.e. makes it easier to use
      OAuth2
    - lets users auth against their google accounts
    - allows "over-the-air" installation of android apps (whatever that is)
1. Plain Oauth2

Android app options 1. Fast Identity Online Universal 2 factor (FIDO U2F) \*
enables physical key support 1. Smart lock for passwords (a password manager
thing shared between android & chrome)

- Uses OAuth2 and OpenID Connect (OIDC) under the hood

With Google Sign-in: _ the user is signed in on the client-side (browser)
**only** _ the googleapi JS is responsible for managing that "signed-in" state
e.g. renewing tokens etc _ the user is NOT signed in on your server _ => the
user does NOT have a session on your server _ your server does not know who the
user is _ the client can send a blob of JSON about the user to the server _
validating the token you get from the client as the server is easy enought _ in
one of googles supported languages there is an package you can load _ ruby and
elixir are not supported so you have to do it manually _ for lower volumes,
doing it manually just involves hitting a google endpoint which will do the work
_ you can do the work manually if you need more speed than hitting the google
endpoint _ after you have validated the token you can sign the user into your
own app

Aside: The "Google API Client Library" for ruby is "alpha quality"

# QUESTION: Should I usedd _Google Sign-in_ or plain OAuth2 to allow users to sign-in to google accounts from a rails/elixir app?

- ++ it is fairly straightforward with google sign-in
- -- that flow only works for google, you can't use the same flow to get idenity
  for facebook or other openid providers

- ?? does the google sign-in api give the user a better experience that manually
  doing an oauth2+openid flow?
