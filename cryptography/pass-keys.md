# Pass keys

## What


## When

* macOS 13, iOS 16
  * add "pass keys" to their platform authenticator
* Android
  * soon I think
* Windows
  * ???


Types of authenticator

1. cross platform authenticator
1. single device authenticator

When the user signs in with their cross platform authenticator, you can prompt them to add a single-device authenticator so they can more easily sign in from that device in future.

* An authenticator and the browser speak CTAP2 protcol to each other
* CTAP2 can run over
  * USB
  * Bluetooth LE
  * NFC
* Example authenticators
  * Cross-platorm authenticators
    * An actual Yubikey hardware key
    * Phone running Android platform authenticator (part of Play Services I think)
    * Phone running iOS 16+ platform authenticator
  * Platform authenticators
    * Windows Hello
    * macOS (13+) platform authenticator


Discoverable credentials = keys which can be sync'd


Currently keys are used as a better second factor
but the only reason we need two factors is because passwords are  so crap
so we can have a secure single factor authentication using WebAuthN!


THere is an "extra secure" variant where the site can check both the sync'd key and a per-device key
site can tell which user is logging and and which device they are on.

An account on a website has one password but can have multiple passkeys

Stored in DB:
  user_id, raw-id, public-key
  user_id, raw-id, public-key
  user_id, raw-id, public-key
  ...

## How does the browser on a laptop and a phone based authenticator talk the first time

1. User wants to sign-up or add a webauthn cred
1. Site shows QR code containing:
    *  ?
1. User scans QR code with their phone
1. phone recognises it as a "webauthn thingy"
1. phone registers a tunnel to google/apple (depending on OS)
  * protocol between phone and tunnel-server is called HYBRID (used to be CABLE2.1)
  * but because the phone and tunnel-server are made by the same entity, this could be proprietary
  * tunnel-server caps the total volume of data that it will transfer
1. phone starts broadcasting a Bluetooth LE advert
1. Laptop reads the BLE advert and uses info to connect to the tunnel
    * now the phone and laptop have a tunnel
1. Both sides use the noise protocol to setup a secure tunnel
1. The laptop and phone speak CTAP2 over the secure tunnel
1. The phone optionally gives the laptop a bunch of keys to let the laptop initiate a connection in future over a tunnel without the QR code

Subsequent times

THe tunnel server can contact the phone to setup a tunnel again if it got the keys from the phone one the first sign-in

Currently Android doesn't have an API where other apps can be providers of pass-keys but Google want to change that.
  This would allow 1Password to work
  They'll probalby need something special on iOS because apps can't set the "system ???" in BLE


## Do you still need a username? If so, why?

You don't need a username if your Authenticator supports resident credentials
Server sends it's "relying party ID" to the authenticator, which uses that choose the right key from it's stored keys
Yubikey 4+ supports this but you must configure a PIN to enable it

In this case the server stores

in config:
  it's relying party ID (probably it's domain name)
in DB for each user
  user_id, raw_id, public_key

You don't need a username or an email! (At least for account purposes)

How does acc recovery work in this case?
  the user does it with Apple/Google/Yubico not you - you can't help them. This seems like a mixed outcome
  I think in most cases you'd want to store an email or name too for other purposes but you don't need to for auth


## You can never change your rp_id without essentially removing all your user's accounts so choose sensibly
This is true for resident credentials, not sure about the older style