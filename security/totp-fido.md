
_This is still a mess_

Protocols

* TOTP
* FIDO U2F
	* developed by Google and Yubikey
	* standard now hosted by the FIDO (Fast IDentity Online) consortium (seems to be mostly Banks and large tech companies)
* the specs emphasize a device centric model
* client can be installed at the OS or the web browser level
* FIDO has some protocols which work over bluetooth and NFC
* WebAuthn is an outgrowth of FIDO's U2F 2.0

For the whole thing to work

1. The website/service must support U2F
1. Your browser must support U2F (currently only Chrome, Firefox, Opera. Edge and Safari are working on it)
1. You must have a U2F device
	* plugs into your computer via USB (mimics a keyboard using HID)
	* The device has a private key embedded at manufacture/provision time
	* You have to trust that the manufacturer won't make duplicates

Prices on 2018-08-15

* Yubikey 4 = $86 NZD
* Yubico security key = $232 NZD

### Yubico devices

* https://www.yubico.com/product/yubikey-4-series/
* Supported protocols:
	* FIDO U2F
	* smart card (PIV)
	* Yubico OTP
	* OpenPGP
	* OATH-TOTP
	* OATH-HOTP
	* and Challenge-Response
* so it supports heaps of protocols




* OTP (One time passwords)
    * the second factor comes from a thing the user carries with them - can be a phone, yubikey etc.
    * downsides
        * somewhat involved for users to set it up
        * manufacturers often posess the seed values for tokens
        * yubikey OTP codes can be 32 chars vs 6
        * secrets need to be stored on servers

> The increasing sophistication of attacks against OTP schemes was a motivating factor in the development of the FIDO U2F protocol

> When a user registers a U2F device with an online service, a public/private key pair is generated

> After registration, when the user attempts to log in, the service provider sends a challenge to the client. The client compiles information about the source of the challenge, among other information. This is signed by the U2F device (using the private key) and sent back to the server (service provider).

Real-time challenge-response schemes like U2F address OTP vulnerabilities such as phishing and various forms of man-in-the-middle attacks. As the legitimate server is issuing the challenge, if a rogue site or middle-man manipulates the flow, the server will detect an abnormality in the response and deny the transaction.

Advantages of U2F include:

Strong security from public key cryptography.
Easy to use with no codes to re-type and no drivers to install.
High privacy so that no personal information is associated with a key.
Unlimited usage in that an unlimited number of accounts can be protected by one single device.


Chrome browser is the only available client

* Firefox now supports it but
    * Mozilla plans to only support the out-of-the-box experience with FIDO U2F devices using Web Authentication APIs (as part of FIDO 2) versus FIDO U2F APIs.

In many ways, FIDO 2 is the next-generation of FIDO U2F, as it will pave the way for things like multi-factor and passwordless login, while still supporting two-factor authentication (2FA) functionalities of the original FIDO U2F standard. As Web Authentication specifications will likely not be complete until early 2018, users will need to wait for the seamless experience with U2F devices in Firefox until the Web Authentication API integration is done.

Firefox 60 is the first browser to support the new security standard, FIDO2, Web Authentication (WebAuthn) and U2F.

> the WebKit team has decided not to officially support U2F, because they consider it a Chrome-only hack, rather than a standardized API. See this and previous messages. They're working on the Web Authentication API instead
