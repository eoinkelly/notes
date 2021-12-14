# Elliptic Curve Cryptography (ECC)

* https://blog.cloudflare.com/a-relatively-easy-to-understand-primer-on-elliptic-curve-cryptography/
* https://andrea.corbellini.name/2015/05/17/elliptic-curve-cryptography-a-gentle-introduction/
* https://research.nccgroup.com/2021/11/18/an-illustrated-guide-to-elliptic-curve-cryptography-validation/


## Overview

* ECC is a public key crypto solution
* a generation newer than RSA and Diffe-Hellman
* 1977 both the _RSA Algorithm_ and _Diffe-Hellman key exchange_ were introduced
* security based on theory of numbers
* first to enable comms between parties **without a shared secret**
* use a pair of keys
    * public key for encrypting data
    * private key for decrypting data
* for crypto you need a set of algorithms that are easy to process in one direction and difficult to undo
    * you kind of have two algorithms in a pair
    * these are called "trap door" functions
    * crypto is all about finding good trap door functions
    * the bigger the gap in difficulty between the functions, the more secure the system

## RSA

* RSA = Ron Rivest, Adi Shamir, Leonard Adleman
* came with a security proof
* multiplies two prime numbers
    * multiplication = the easy algorithm
    * factoring = the difficult algorithm
* the public and private keys are each numbers 1 < x < max
* `max`is calculated by multiplying two random primes together
* Given a plaintext e.g. a number
* to encrypt: multiply it by itself `pub` times, wrapping around `max`if the result
* to decrypt: multiply the number by itself `priv` times and you get back to the original number
* weird that it works but it does :shrug:

To create a key

1. randomly pick two prime numbers to obtain the `max`
2. pick a number to be the public key
3. Knowing the two prime numbers and the `pub` number you can derive the `priv` number

### Anatomy of a key

```bash
$ openssl rsa -in ./private_key.pem
```

has the following output (removed actual numbers for brevity):

```
Private-Key: (2048 bit)
modulus:
    <hex string here>
publicExponent: 65537 (0x10001)
privateExponent:
    <hex string here>
prime1:
    <hex string here>
prime2:
    <hex string here>
exponent1:
    <hex string here>
exponent2:
    <hex string here>
coefficient:
    <hex string here>
```

    Question: When a string is encrypted is it encrypted byte by byte? or in bigger chunks?

### Weakness in RSA

If you can factor `max` into it's component primes then you can derive `priv` from `pub`.
So if we find better ways of factoring then RSA becomes weaker

Factoring algorithms such as

* Quadratic Sieve
* General number field sieve

get more efficient the larger the numbers get. This means the bit length of keys
needs to increase faster than the available computing power to stay secure.

An ideal trapdoor function would have the property the easy path and the hard
path get harder at the same rate as the numbers increase in size.

> the gap between the difficulty of factoring large numbers and multiplying
> large numbers is shrinking as the number (i.e. the key's bit length) gets
> larger.

## Elliptic curves

* proposed in 1985
* researchers were searching mathematics for functions which had the properties of a good trapdoor noticed elliptic curves

> technically an elliptic curve is the set points satisfying an equation in two
> variables with degree two in one of the variables and three in the other

$$y^2=x^3 - ax + b$$

Aside: https://www.desmos.com/calculator is useful here

Properties of elliptic curves which are useful for crypto:

* they have horizontal symmetry (mirrored around the x-axis)
    * if you transform any point on the curve through the x-axis, the result will also be a point on the curve
* any non-vertical line will intersect the curve in **at most** 3 places

The "dot" operation

1. Draw a line joining any two points on the curve A and B
1. Extend it until it hits the curve again, then reflect that point across the x axis to get point C

```
A dot B = C
```

The `dot` operation in EC is used similarly to how the multiplication operation is used in RSA

```
A dot A = B
A dot B = C
A dot C = D
etc.
```

    how is the first dot done?  somehow a point can be dotted with itself?

If you know

1. the exact equation of the curve
1. the initial point A
1. the final point Z

it is still very difficult to know how many times A was dotted to get Z so it is a good trapdoor function.

We still need to limit the range of numbers we allow using the same trick as RSA
(by only allowing whole numbers and wrapping the result at the chosen `max`)

> If we pick the maximum to be a prime number, the elliptic curve is called a
> prime curve and has excellent cryptographic properties

THe wrapping and limiting to whole numbers turns the graph from a continuous line to a set of discontinuous points but the rules still apply - any line drawn between two points will hit a third

To create an EC cryptosystem

1. Pick a prime number as `max`
2. Pick a curve equation
3. Pick a point on the curve to be your "public" point on the curve
4. Pick a private key number `priv`
5. The public key is the public point dotted with itself `priv` times

This is called an Elliptic curve discrete logarithm function

This is the hard problem underpinning EC cryptography

> Despite almost three decades of research, mathematicians still haven't found
> an algorithm to solve this problem that improves upon the naive approach.

> In other words, unlike with factoring, based on currently understood
> mathematics there doesn't appear to be a shortcut that is narrowing the gap in a
> Trapdoor Function based around this problem. This means that for numbers of the
> same size, solving elliptic curve discrete logarithms is significantly harder
> than factoring. Since a more computationally intensive hard problem means a
> stronger cryptographic system, it follows that elliptic curve cryptosystems are
> harder to break than RSA and Diffie-Hellman.

> To visualize how much harder it is to break, Lenstra recently introduced the
> concept of "Global Security." You can compute how much energy is needed to break
> a cryptographic algorithm, and compare that with how much water that energy
> could boil. This is a kind of cryptographic carbon footprint. By this measure,
> breaking a 228-bit RSA key requires less energy to than it takes to boil a
> teaspoon of water. Comparatively, breaking a 228-bit elliptic curve key requires
> enough energy to boil all the water on earth. For this level of security with
> RSA, you'd need a key with 2,380-bits.

> With ECC, you can use smaller keys to get the same levels of security. Small
> keys are important, especially in a world where more and more cryptography is
> done on less powerful devices like mobile phones.

### Algorithm choices

* Elliptic curves are widely used in cryptography now
* They are a "2nd generation" of algorithms, replacing RSA and Diffe-Hellman

ALgorithm choices for public key crypto:

* DSA = ancient, do not use
* RSA = should avoid now
* ECDSA
    * It depends on how well your machine can generate a random number that will be used to create a signature.
    * There's also a trustworthiness concern on the NIST curves that being used by ECDSA
    * ECDSA requires a good source of entropy unlike RSA so it is vulnerable to that having bugs e.g. there was a bug on Android
* Ed25519:
    * Allegedly the most recommended public-key algorithm available today
    * introduced on OpenSSH version 6.5.
    * It's the EdDSA implementation using the Twisted Edwards curve https://en.wikipedia.org/wiki/Twisted_Edwards_curve
    * short public keys - 68 characters
* NIST approved prime-curves P-256 and P-384
    * Some questions over whether these have been backdoored by NSA
* P-521
    * Chrome has dropped support but I'm not sure why https://bugs.chromium.org/p/chromium/issues/detail?id=478225
    * Firefox doing the same
    * Apparently doesn't provide extra security over P-256

### Use-cases

* SSH
    * Totally usable right now I think - been there since Openssh 6.5 which was ages ago.
* TLS key exchange with EC
    * Supported by all modern browsers
    * https://wiki.mozilla.org/Security/Server_Side_TLS has good advice for TLS setup
    * Conclusion: focus on TLS level support and don't worry about the specific algorithms - they are constrained by the TLS version
* TLS certificates (X509)
    * you have to submit a different kind of CSR (an ECC-CSR)
        * https://www.digicert.com/kb/ecc-csr-creation-ssl-installation-apache.htm
    * cloudflare.com is signed with EC secp256r1, the cloudflare intermediate is also signed iwth EC but the root cert is signed w RSA
        * so they can be mixed
    * amazon.com signed with RSA all the way up the chain
    * google.com is EC but the intermediate and the root are RSA 2048
    * Opinion: it still seems a bit off the beaten path to get an EC cert in 2021. Probably not worth doing.


* X25519 (ECDH Key Exchange)
    * X25519 is the name used for key exchange (Diffie-Hellman) which has been widely supported for some years now since it was added to TLS.
* ED25519 (Digital signatures)
    * Ed25519 is the name used for digital signatures (Ed is short for Edwards) which is what you need for certificates. It was a few years later to arrive in OpenSSL because TLS didn't need it.

To be "fully EC", your site's cert, all the intermediate certs up the root cert have to be EC but you don't control the intermediates and root so it's not a goal to worry about.



```
# generate an ECC-CSR
# Q: is prime256v1 the only choice? best choice?
openssl ecparam -out server.key -name prime256v1 -genkey
openssl req -new -key server.key -out server.csr -sha256
```

https://ianix.com/pub/ed25519-deployment.html describes "modern crypto" as X25519, Ed25519 and ChaCha20-Poly1305

Seems like ED25519 might be the people's choice of EC because the NIST approved ones are suspected of being compromised by NSA

### Speed

EC crypto is much faster than RSA.

> The performance improvement of ECDSA over RSA is dramatic. Even with an older
> version of OpenSSL that does not have assembly-optimized elliptic curve code, an
> ECDSA signature with a 256-bit key is over 20x faster than an RSA signature with
> a 2,048-bit key

EC sig validation is more CPU intensive even though it is faster - this can be an issue for low power devices

### Examples

This is Cloudflare & Google's curve for TLS

```
max: 115792089210356248762697446949407573530086143415290314195533631308867097853951
curve: y² = x³ + ax + b
a = 115792089210356248762697446949407573530086143415290314195533631308867097853948
b = 41058363725152142129326129780047268409114441015993725554835256314039467401291
```

### Gotchas

* Not all curves are good choices
* Patents have held back adoption of ECC