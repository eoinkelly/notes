# WebAuthn and Passkeys

- [WebAuthn and Passkeys](#webauthn-and-passkeys)
  - [Sources](#sources)
  - [Best practices](#best-practices)
  - [Overview](#overview)
  - [What are the benefits?](#what-are-the-benefits)
  - [What do I need](#what-do-i-need)
  - [Can I use it yet?](#can-i-use-it-yet)
  - [Implementing the Relying Party in Rails](#implementing-the-relying-party-in-rails)
    - [Apple Passkeys = synced WebAuthn credentials across devices](#apple-passkeys--synced-webauthn-credentials-across-devices)
  - [Pass keys](#pass-keys)
    - [How does the browser on a laptop and a phone based authenticator talk the first time](#how-does-the-browser-on-a-laptop-and-a-phone-based-authenticator-talk-the-first-time)
    - [Do you still need a username? If so, why?](#do-you-still-need-a-username-if-so-why)
    - [You can never change your rp\_id without essentially removing all your user's accounts so choose sensibly](#you-can-never-change-your-rp_id-without-essentially-removing-all-your-users-accounts-so-choose-sensibly)
    - [Jan 2023 experience report](#jan-2023-experience-report)
      - [Summary](#summary)
      - [Details](#details)

## Sources

* https://webauthn.io/ (guide & intro)
* https://w3c.github.io/webauthn/ (spec)
* https://passkeys.dev/device-support/

## Best practices

* Provide normal credentials too since the webauthn private key is tied to a particular device so user could get locked out if they lose the device
    * Apple mitigates this by saving the keys to your iCloud KeyChain so they are sync'd between devices
* Use feature detection to detect the device supports it existence
* Remember enrollment with a secure cookie so that the browser can just sign-in the user using it in future
* If you already support security keys, consider whether you will confuse users by also presenting webauthn
    * what does "a website supporting security keys" without webauthn mean?

## Overview

* allows servers to register and authenticate users using public key crypto instead of a password
* spec created by W3C and FIDO. Contributions from Google, Mozilla, MS, Yubico, others
* Part of the FIDO2 framework

During registration:

1. a keypair is created for the website (diff keypair for each website)
2. private key stays on user's device, public key sent to the website for storage as part of registration
    * => server gets no secret, hacking the server DB doesn't give you a usable secret


> WebAuthn is supported in the Chrome, Firefox, and Edge browsers to different
> degrees, but support for credential creation and assertion using a U2F Token,
> like those provided by Yubico and Feitian, is supported by all of them

## What are the benefits?

* Server does not store password. Server stores a public key which is used only for that service so hacked databases are much less useful
* Don't have to worry about users picking crap passwords
* Better UX for users because they don't have to rotate passwords or meet complexity requirements

## What do I need

1. A _Relying Party_
   * Server must support being a _Relying Party_ (Rails has this, presumably others too)
2. A _User agent_
    * Browser must support the _Web Authentication API_ (all modern browsers do)
    * Native apps can be user agents too
3. An _Authenticator_
    * Authenticator must provide a way to
      * generate keypairs during registration
      * manage keypairs that is easy for users
      * participate in authentication
    * safely stores private keys for the user (one private key per website I **think** TODO: confirm)
    * built into a single computing device aka "Platform authenticator"
        * Apple TouchID or FaceID
            * naturally 2factor: something you have (the iphone), something you know/are (your face/fingerprint)
        * Windows Hello
        * Android fingerprint reader (not sure about this one)
    * built into special hardware that can roam from device to device
        * Yubikey
    * Q: can the browser be "the authenticator"?
    * 1Password is working on support for it. Unclear whether that support is for apple platforms only


## Can I use it yet?

This needs a lot more than browser support. The user needs sensible ways of
managing passkeys. This can be done by the operating system. Maybe the browser
can also do it? Not sure about that yet.

* Browsers must support the _Web Authentication API_ to support WebAuthN - https://caniuse.com/?search=webauthn good support from all modern browsers now

iOS 16 and macOS Ventura will bring this closer to being usable.

* Safari 15 and older have a "Platform authenticator" which is **not the same as passkeys**
    * it lets you use faceid and touch ID with websites using WebAuthN
    * but your keys are **not shared between your devices - they are only on the device you created it on**
    * I think I can delete them from Safari's develop menu but I can't find any UI to manage them or even see what's in there
    * TL;DR: Confusing, do not use.
    * Rememember that while you have only one password on a website, you will probalby have many passkeys.
    * The intention is that you have a password for general access but can use webauthn if you are on a device that you have setup to use it (for faster access)
      * I believe this works with GithHub today.

## Implementing the Relying Party in Rails

* https://github.com/cedarcode/webauthn-ruby allows ruby/rails to be _Relying party_

Attestation type: none|direct|indirect
Authenticator type: unspecified|cross-platform|platform-tmp

> You must use  JS on the client to do the sign-in - can't just use forms. there is a JS api to do the login
Is this true?

Tips

* Your username input in HTML should have `autocomplete="username webauthn"` annotations to let the browser prompt for autofill from passkeys.
    * Does this only work on Safari?


### Apple Passkeys = synced WebAuthn credentials across devices

https://developer.apple.com/videos/play/wwdc2022/10092/ (shipping in macOS Ventura and iOS 16)
https://developer.apple.com/videos/play/wwdc2021/10106/ (preview announced)

## Pass keys

Types of authenticator

1. cross platform authenticator
1. single device authenticator

When the user signs in with their cross platform authenticator, you can prompt them to add a single-device authenticator so they can more easily sign in from that device in future.

* An authenticator and the browser speak CTAP2 protocol to each other
* CTAP2 can run over
  * USB
  * Bluetooth LE
  * NFC
* Example authenticators
  * Cross-platform authenticators
    * An actual Yubikey hardware key
    * Phone running Android platform authenticator (part of Play Services I think)
    * Phone running iOS 16+ platform authenticator
    * macOS (13.1+) platform authenticator (put in icloud keychain passwords)
  * Platform authenticators
    * Windows Hello

Discoverable credentials = keys which can be sync'd to multiple devices.

Servers can specify that they don't allow discoverable credentials (i.e. disallow cross-platform)

TOTP is currently used as a second factor but the only reason we need two
factors is because passwords are so crap so we can have a secure single factor
authentication using WebAuthN!

THere is an "extra secure" variant where the site can check both the sync'd key
and a per-device key site can tell which user is logging and and which device
they are on.

An account on a website has one password but can have multiple passkeys

```
Stored in DB:
  user_id, raw-id, public-key
  user_id, raw-id, public-key
  user_id, raw-id, public-key
  ...
```

### How does the browser on a laptop and a phone based authenticator talk the first time

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


### Do you still need a username? If so, why?

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


### You can never change your rp_id without essentially removing all your user's accounts so choose sensibly
This is true for resident credentials, not sure about the older style


### Jan 2023 experience report

#### Summary

* Update: I found https://passkeys.dev/device-support/ after I did my own testing. Use this in future.
    * See also https://passkeys.dev/docs/reference/macos/#browser-behavior which mirrors what I found
* Support is progressing
* Passkeys are silo'd in their storage and it's difficult/impossible to use them if the storage doesn't support them
* Apple's storage is probably the most useful
* Things missing
  * 1password supporting it fully (due soon)
  * being able to export/import passkeys from different storage would be nice but because you can have multiple keys for each account it's not vital
  * I think the UX is still fussy enough to make this confusing for normal folk
* They are currently _mostly_ a better second factor (where you don't have to buy a USB thing)
* Thoughts for server side stuff
    * Users can/will have multiple passkeys associated with their account
        * we should give them option to give the keys nicknames so they can add/remove them later on


#### Details

Places I can store passkeys:

1. Apple iCloud Passwords
    * When you export passwords from Settings app, passkeys are not exported
    * Passkeys and legacy username+passwords are shown in the same list
   * When I visit https://webauthn.io/ in Safari,
       * I am prompted to use the Apple passkey
       * I can use a passkey which exists on a different Apple device by scanning a QR code
    * When I register a passkey in Safari
        * I'm prompted to save it into my apple store
        * that passkey is synced to all my devices
2. Chrome
   * When I visit https://webauthn.io/ in Chrome
     * I am prompted with the Chrome passkey UI
     * I can scan a QR code and use the passkey from my Apple passwords
     * It seems I can use a USB device too if it supports passkeys
     * to use the Chrome passkey but not the apple on
    * When I register a passkey in Chrome
        * option 1:
            * save it as a single-device key
            * the passkey is saved to my laptop but not synced to my Google account - see `chrome://settings/passkeys?search=passkey`
        * option 2:
            * Save a different way which pops up the QR code.
            * I can scan that QR with my iphone and then the passkey is in my apple store
3. Firefox (no support yet)
4. Windows (I can't test this)


I have a passkey on my laptop stored in the apple store. Can I use this in Chrome without having another apple device which can scan a QR?
I haven't found a way to do this.

Chrome supports cross-device syncing using the Android password manager I think
but I couldn't create a passkey which would be shared between multiple devices on macOS chrome (unless i used the QR code to put it in the apple store)

> On Android, Google Password Manager is keeping passkeys safe and synced, but future versions of the platform will provide support for other password managers that support passkeys as well, Google says.
>
> https://www.securityweek.com/passkeys-now-fully-supported-google-chrome

I created a passkey for github (stored in apple storage)
Github uses it as a second factor but not as a replacement for passwords
This is a bit confusing because it's not super clear in the UI