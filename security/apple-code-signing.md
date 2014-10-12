
Th _signing identity_ consists of a public-private key pair _that Apple issues_.

Any `.cer` downloaded from Apple dev site contains only _public key_ and _identity info about me as a member of Apple developer program_ . It does not include a private key so I presume:

1. Xcode makes a new public+private key pair
2. Xcode creates a CSR and sends to Apple
3. Apple sends back signed cert which xcode puts in keychain

    QUESTION: what exactly is in a CSR?

Certs that come from Apple have X509v3 extensions that indicate to Apple
software what the intended usage of this certificate is e.g.

    X509v3 Key Usage: critical
        Digital Signature
    X509v3 Extended Key Usage: critical
        Code Signing

All Apple code stuff is signed by the _Worldwide Developer Relations Cert_
(https://developer.Apple.com/certificationauthority/AppleWWDRCA.cer) - this
must be installed in my keychain for any of this to work. This in turn is
signed by the _Apple Root CA_ cert which comes installed on all Apple devices.


