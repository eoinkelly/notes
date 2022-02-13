# Web Authentication API aka WebAuthn

## Best practices

* Provide normal creds too since the webauthn private key is tied to a particular device so user could get locked out if they lose the device
* Use feature detection to detect the device supports it existance
* Remember enrollment with a secure cookie so that the browser can just sign-in the user using it in future
* If you already support security keys, consider whether you will confuse users by also presenting webauthn
    * what does "a website supporting security keys" without webauthn mean?

## Overview

* allows servers to register and authenticate users using public key crypto instead of a password
* spec created by W3C an d FIDO. Contributions from Google, Mozilla, MS, Yubico, others
* Part of the FIDO2 framework

during registration

1. a keypair is created for the website (diff keypair for each website)
2. private key stays on user's device, public key sent to the website for storage as part of registration
    * => server gets no secret, hacking the server DB doesn't give you a usable secret

todo

what is it
rails support

https://webauthn.io/

> WebAuthn is supported in the Chrome, Firefox, and Edge browsers to different degrees, but support for credential creation and assertion using a U2F Token, like those provided by Yubico and Feitian, is supported by all of them

https://caniuse.com/?search=webauthn
good support from all modern browsers now


https://github.com/cedarcode/webauthn-ruby
allows ruby/rails to be _Relying party_
you alos need a (User agent + authenticator) pair$$


user-agent = a browser or some device capable of talking to the website and to the authenticator

authentiator = some way of proving identity e.g.

* safely stores private keys for the user (one private key per website I **think** TODO: confirm)
* built into a single computing device aka "Platform authenticator"
    * Apple TouchID or FaceID
        * naturally 2factor: something you have (the iphone), something you know/are (your face/fingerprint)
    * Windows Hello
    * Android fingerprint reader (not sure about this one)
* built into special hardware that can roam from device to device
    * Yubikey


I think the store is not in the browser???
    the authenticator is NOT the browser


How do I debugm


Attestation type: none|direct|indirect
Authenticator type: unspecified|cross-platform|platform-tmp

You must use  JS on the client to do the sign-in - can't just use forms
there is a JS api to do the login

https://developer.apple.com/videos/play/wwdc2020/10670/

Attestation

* Apple as built it's own attestation service so that every website gets a different attestion cert to prevent user tracking across sites
    * This is "coming soon" and called "Apple anonymous attestion"


### Apple Passkeys = synced WebAuthn credentials across devices

https://developer.apple.com/videos/play/wwdc2021/10106/