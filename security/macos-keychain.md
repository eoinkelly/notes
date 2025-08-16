# Keychain Services

Sources:

- `http://en.wikipedia.org/wiki/Keychain_(Apple)`
- https://developer.apple.com/library/mac/documentation/Security/Conceptual/keychainServConcepts/02concepts/concepts.html#//apple_ref/doc/uid/TP30000897-CH204-TP9

- The name of the GUI app is _Keychain Access_
- Command line equivalent of _Keychain Access_ is `/usr/bin/security` (describes
  itself as a command line interface to keychains and Security framework)

It is open source ??? TODO check this

In Mac OS X, keychain files are stored in

```
~/Library/Keychains/
/Library/Keychains/
/System/Library/Keychains/

$ ls -l /Library/Keychains
-rw-r--r--  1 root  wheel  104144 13 Sep 13:20 System.keychain
-rw-r--r--  1 root  wheel   20460 24 Oct  2013 System.keychain-orig
-rw-r--r--  1 root  wheel   50640 12 Jun 21:26 apsd.keychain

$ ls -l /System/Library/Keychains
total 1840
-rw-r--r--  1 root  wheel    7008 24 Sep 11:52 EVRoots.plist
-rw-r--r--  1 root  wheel  196808 24 Sep 11:52 SystemCACertificates.keychain
-rw-r--r--  1 root  wheel  513324 24 Sep 11:52 SystemRootCertificates.keychain
-rw-r--r--  1 root  wheel  113666 24 Sep 11:52 SystemTrustSettings.plist
-rw-r--r--  1 root  wheel  282984 24 Oct  2013 X509Anchors

$ tree ~/Library/Keychains
/Users/username/Library/Keychains
├── <big long uuid thing>
│   ├── accountStatus.plist
│   ├── keychain-2.db
│   ├── keychain-2.db-corrupt
│   ├── keychain-2.db-shm
│   ├── keychain-2.db-wal
│   └── user.kb
├── login.keychain
└── metadata.keychain
```

A keychain is an encrypted container that holds

- passwrods
- crypto keys
- certs
- text notes (not on iOS)

Humans use _Keychain Access Utility_ to access keychains. Apps use _Keychain
Servcies_ to access keychains.

What keychains are created

- login.keychain = default auto created keychain for an account on the system
    - automatically unlocked when the user unlocks the system iff it has the
      same password as the user account => you should change password of the
      keychain
    - is _default keychain_ so new items are put there by default
- /System/Library/System.keychain
    - The system keychain is stored in `/Library/Keychains/System.keychain` and
      the key to unlock it is stored in `/var/db/SystemKey` (its default file
      permissions are readable by root only)
    - `/var/db/SystemKey` is a binary file

# Keychain on iOS

- There is a single keychain on the system that stores all items for all apps
- An iOS app can access its own keychain items but not items from any other app
- In iOS the user does not have to enter keychain password
- the system generates its own password for the keychain
- when you backup your iphone the keychain is backed up but the password is not
  included in the backup
    - means an attacker cannot get access if they have a backup

keychains can be locked on MacOS but not iOS

# Structure of a keychain

- A _keychain_ contains _keychain items_.
- A _keychain item_ contains _data_ and _attributes_.
- Attributes are never encrypted so can always be read (even if keychain is
  locked)
- Some keychain items have their _data_ encrypted, some do not.
- Each item and the keychain itself has 1 _access object_ associated
- An _access object_ contains _access control list entries_ (ACL entries)
- ACL entry contains _authorization tags_ and a _set of trusted applications_.
- The ACL is looked up whenever an app tries to CRUD an item

There are some standard attributes exposed by the UI

1. Name
2. Kind
3. Account
4. Where
5. Comments

# Item classes

Items are grouped into _classes_ (labeled as "kind" in the UI). The "kind" field
is free form text so you can add anything you want to it.

These are the default classes used by Apple:

1. Internet password
2. generic passwords
3. network passwords
4. AirPort network password
5. iTunes iAd password
6. certificate
7. Application password
8. Airplay Client Peer
9. Airplay Client Identity
10. Token
11. private key
12. public key
13. secure note

You can think of classes/kinds as a "tag" on the item.

# iCloud keychain

- Only works with 1 browser: safari
- Needs the app developer to have specifically enabled it for an app on iOS
- Hard to copy & paste them from iOS UI to fill passwords in manually
- Secured with a 4-digit code or passphrase
    - only asked for it on 1st device

# Practical stuff

_Edit > Change Settings for Keychain_ to control whether a Mac OS keychain will
lock on sleep or after timeout.

- Dump info about keychain: sudo security dump-keychain /path/to/some.keychain
- You can import from items from one keychain to another using the UI.
- You only can add `application password` and `secret note` classes of items
  from the UI.
- The "Ticket viewer" is for managing _Kerberos Tickets_ - it is a GUI over MIT
  Kerberos tools like `kinit` `klist`, `kdestroy`.

# Certificates vs My Certificates

Q: What is diff between _Certificates_ and _My Certificates_ in keychain access?

A: _My Certificates_ is the subset of _Certificates_ that I also have associated
private keys for.

# Import and export

- Formats _Keychain Access_ can import:
    - ???

- Formats _Keychain Access_ can export:
    - Application passwords -> No export
    - Secure notes -> No export
    - RSA public keys -> PEM format

```
snippet of security man page
    security import -t somekey -t ... -f ...
            -t type         Specify the type of items to import. Possible types
                            are cert, pub, priv, session, cert, and agg. Pub,
                            priv, and session refer to keys; agg is one of the
                            aggregate types (pkcs12 and PEM sequence). The com-
                            mand can often figure out what item_type an item
                            contains based in the filename and/or item_format.
            -f format       Specify the format of the exported data. Possible
                            formats are openssl, bsafe, raw, pkcs7, pkcs8,
                            pkcs12, x509, openssh1, openssh2, and pemseq. The
                            command can often figure out what format an item is
                            in based in the filename and/or item_type.
```

## Task: Import SSH keypair into Keychain Access

```
# Make an SSH key-pair - save the keys in: testkey, testkey.pub
ssh-keygen -b 4096 -t rsa -C "test rsa 4096 key"

# Import the public key:
security import ./testkey.pub -k eoins_playground.keychain -f openssh2 -t pub

# Import the private key:

# you cannot import the RSA private key directly into keychain
# you have to convert it before keychain will inport it

how to convert from openssh2 RSA private key to p12 format??
openssl pkcs12 -export -clcerts -inkey private.key -in certificate.crt -out MyPKCS12.p12 -name "Your Name"

TODO how???

```

## Task: Import GnuPG keypair into Keychain Access

Gnupg Keychain access only export format is an base64 encoded file (`.asc`
extension) which contains the public and private key in ??? format ???

# Exporting certificates and keys from keychian

When you export a `.cer` from keychain you export a X509 certificated in DER
format. It does not contain the private key. You can inspect the file with

```
openssl x509 -inform der -noout -text -in file.cer
```

Whe you export a `.p12` you get a PKCS12 certificate, encrypted iwth a password
that contains both public and private key. You can inspect the file using

```
openssl pkcs12 -in file.p12 -info
# you will be prompted for password for the file and then a password for the private key
```

You can export a single public key as `PEM` file.
