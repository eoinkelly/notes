# Open-PGP

Sources

* https://latacora.micro.blog/2019/07/16/the-pgp-problem.html

## Alternatives

* Secure messaging
    * Signal, Wire, WhatsApp or some other Signal-protocol-based secure messenger
* Encrypted email
    * Don't do it if secrecy really matters. It's too hard for everybody involved to get it right all the time.
* Sending files in realtime
    * Magic Wormhole if you are communicating with nerds
        * https://github.com/magic-wormhole/magic-wormhole
    * Age
        * if you need to encrypt a file and send it over an existing secure channel
    * Signal can do secure file transfer for everybody else
* Encrypting file backups
    * Tarsnap
* Encrypted disk image backups
    * Use the encrypted disk image stuff built into your OS
* Full-disk encryption
    * Use the stuff built into your OS
* Signing packages
    * Signify/Minisign (used by OpenBSD)
* Encrypting application data
    * Use libsodium.
        * Had bindings for many libs
* Encrypting files
    * Use age https://github.com/FiloSottile/age
        > Age is, of course, much younger than PGP. But I would bet all the
        > money in my pocket against all the money in yours that a new
        > vulnerability will be found in the clangorous contraption of PGP before
        > one is found in age. Look into age!

## Why is it bad?

* PGP can be
    1. The OpenPGP standard
    2. The standard's reference implementation in GnuPG
* The cryptography community has largely given up on it
* It was designed in the 1990's "before serious modern cryptography".
* It has a bunch of known weaknesses which have gone unaddressed for decades
* it is really really complex
	* packet format is very complex
		* it is packet based.
		* there are 8 ways of encoding the length of a packet
		* multiple packet formats
		* packets can have sub-packets
		* GnuPG had a keyserver bug where it went into quadratic time parsing keys
	* key format is complex
    	* keys can have sub-keys
    	* multiple kinds of key-rings
    	* revocation certificates
		* multiple types of keys (sign-only, encrypt-only)
    	* 3 different compression formats
	* smart-card support
* it is "mired in backwards compatibility
	* defaults to very outdated ciphers and key lengths
	* if you choose good ciphers, then a lot of the ecosystem might not be able to read your messages
	* the standard tries to address it by addng RFCs to add new ciphers but that doesn't necessarily make it to the install base
	* best summary from article:
		> PGP supports ElGamal. PGP supports RSA. PGP supports the NIST P-Curves. PGP
		> supports Brainpool. PGP supports Curve25519. PGP supports SHA-1. PGP supports
		> SHA-2. PGP supports RIPEMD160. PGP supports IDEA. PGP supports 3DES. PGP
		> supports CAST5. PGP supports AES. There is no way this is a complete list of
		> what PGP supports.
		>
		> If we’ve learned 3 important things about cryptography design in the last 20
		> years, at least 2 of them are that negotiation and compatibility are evil. The
		> flaws in cryptosystems tend to appear in the joinery, not the lumber, and
		> expansive crypto compatibility increases the amount of joinery. Modern protocols
		> like TLS 1.3 are jettisoning backwards compatibility with things like RSA, not
		> adding it. New systems support just a single suite of primitives, and a simple
		> version number. If one of those primitives fails, you bump the version and chuck
		> the old protocol all at once.
        >
        > If we’re unlucky, and people are still using PGP 20 years from now, PGP will be
        > the only reason any code anywhere includes CAST5. We can’t say this more clearly
        > or often enough: you can have backwards compatibility with the 1990s or you can
        > have sound cryptography; you can’t have both.
* it has bad UX
	* yup
* it encourages long-term secrets and makes changing secret high-effort and high-stakes
	> Long term keys are almost never what you want. If you keep using a key, it
	> eventually gets exposed. You want the blast radius of a compromise to be as
	> small as possible, and, just as importantly, you don’t want users to hesitate
	> even for a moment at the thought of rolling a new key if there’s any concern at
	> all about the safety of their current key.
* PGP's identity stuff doesn't work
	* it pushes the idea of a long-term cryptographic identity
    * key signing parties, web of trust, long term keys etc. - all fine in theory but almost never properly followed in practice.
* it leaks metadata
	> Forget the email debacle for a second (we’ll get to that later). PGP by
	> itself leaks metadata. Messages are (in normal usage) linked directly to key
	> identifiers, which are, throughout PGP’s cobweb of trust, linked to user
	> identity. Further, a rather large fraction of PGP users make use of
	> keyservers, which can themselves leak to the network the identities of which
	> PGP users are communicating with each other.
* No forward secrecy (if you lose your key today, attacker should not be able to read past messages)
	* Modern crypto assumes the attacker is recording everything in long-term storage
* Key management is really clumsy
    * Compare it to SSH. Yup.
* GnuPG code is "janky"
* It's a mediocre swiss-army knife of cryptography
    > PGP does a mediocre job of signing things, a relatively poor job of encrypting
    > them with passwords, and a pretty bad job of encrypting them with public keys.
    > PGP is not an especially good way to securely transfer a file. It’s a clunky way
    > to sign packages. It’s not great at protecting backups. It’s a downright
    > dangerous way to converse in secure messages.
    >
    > Modern cryptography doesn’t work like this; it’s purpose built. Secure
    > messaging wants crypto that is different from secure backups or package signing.
