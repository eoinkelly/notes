
Sources

* https://medium.com/ios-os-x-development/ios-code-signing-provisioning-in-a-nutshell-d5b247760bef

## The Intermediate certificate

* The "Worldwide Developer Relations Certificate Authority" cert is installed
  in Keychain when you install Xcode and is trusted by your computer.
* You can re-download it from the certs/profiles area of developer.apple.com
* The keypair associated with this cert is what Apple uses to sign my _signing
  identity_ cert after I (or XCode) uploads it.

All Apple code stuff is signed by the _Worldwide Developer Relations Cert_
(https://developer.Apple.com/certificationauthority/AppleWWDRCA.cer) - this
must be installed in my keychain for any of this to work. This in turn is
signed by the _Apple Root CA_ cert which comes installed on all Apple devices.


## The "Signing identity"

* is a public & private keypair. The public key has been uploaded to apple (in
  a CSR) and signed by their "Worldwide developer relations certificate
  authority" key.

### How do I create a "signing identity"?

1. Create a "Certificate Signing Request" (CSR) in Keychain
    * This will create a private key and a CSR
1. Upload your CSR to apple via the "member center"
1. Apple approves the request and issues you a certificate
    * the certificate contains the public key that corresponds to your private key from first step
    * Any `.cer` downloaded from Apple dev site contains only _public key_ and
      _identity info about me as a member of Apple developer program_ . It does
      not include a private key.

Xcode will do this for you automatically when you install it.

### Where is my "signing identity" stored?

* The private key is locally on your machine only (in Keychain)
* The public key (wrapped in a certificate) is stored locally in Keychain and in the _Apple Member Center_


### How does XCode automate this whole process?

I presume:

1. Xcode makes a new public+private key pair
2. Xcode creates a CSR and sends to Apple
3. Apple sends back signed cert which xcode puts in keychain


Certs that come from Apple have X509v3 extensions that indicate to Apple
software what the intended usage of this certificate is e.g.

    X509v3 Key Usage: critical
        Digital Signature
    X509v3 Extended Key Usage: critical
        Code Signing


## Provisioning profiles

* are PKCS#7 format file
* are basically signed plist files (uncrypted bits viewable in text editor)
* have `.mobileprovision` extension
* live in `~/Library/MobileDevice/Provisioning Profiles`
    * default file name used by Xcode is {some UUID???}
* are all ~ 10 KB
* Xcode will go through all the provision profiles installed on your system and
  pull the developer certs out of them and put them in the keychain.

* contents
    * app ID
    * Team name
    * ProvisionedDevices
        * device UUIDs
    * Time to live
        * how many days the provisioning profile is valid.
        * Note: by default provisioning profiles only last 365 days
    * entitlements
        * a dictionary of key values relating to iCloud e.g.
            * Game center
            * Push notifications
    * an array of developer certificates. Each one is
        * radix64 encoded
        * x509 format
    * expiry date
  all of which are signed by apple

Consequences:

    * you cannot edit a provisioning profile by hand without invalidating the signature.
    * Apple has chosen to use DER not PEM for the signature

http://www.doubleencore.com/2013/04/what-is-a-provisioning-profile-part-1/

### Tasks

To verify and dump unencrypted contents of a .mobileprovision

```
openssl smime -inform der -verify -in ~/Library/MobileDevice/Provisioning Profiles/your.mobileprovision
```

To decode a developer cert from a provisioning profile we first dump the
mobileprovision data:

```
openssl smime -inform der -verify -in ~/Library/MobileDevice/Provisioning Profiles/your.mobileprovision
```

then paste one of the certificates from the output into a file called `foo.pem`

```
-----BEGIN CERTIFICATE-----
{paste the radix64 encoded data here}
-----END CERTIFICATE-----
```

then use openssl to interpret it

```
openssl x509 -text -in foo.pem
```

QUESTION: What is relationshiop of smime and pkcs7?
ANSWER: http://security.stackexchange.com/a/41413
