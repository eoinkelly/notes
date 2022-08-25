# Web Authentication API aka WebAuthn

## Sources

* https://webauthn.io/ (guide & intro)
* https://w3c.github.io/webauthn/ (spec)

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