# MAC Message authentication code

* https://en.wikipedia.org/wiki/Message_authentication_code
* short piece of info used to authenticate a message
    * verify message came from a particular sender
    * verify the contents of message have not changed

MAC vs Cryptographic hash functions

```bash
## Hash
# * hash functions take a single input
# * any change any of the inputs will create a different output
# * no key required
hash_func(msg) -> hash

## MAC
# * mac functions take two inputs, the message and a secret key
# * any change any of the inputs will create a different output
# * verifier must also have secret key (=> some key exchange required)
mac_func(msg, secret_key) -> mac

## Digital signature
# output of the hash function is encrypted using private key to create the signature
encrypt(hash_func(msg), private_key)
```

* hash code can be used to
    1. verify the message was not changed
* MAC code can be used to
    1. verify the message was not changed
    1. verify the message was sent by a holder of the secret key

MAC functions have different security requirements to hash functions. To be considered secure, a MAC function must _resist existential forgery under chosen-message attacks_

This means that even if an attacker has access to an oracle which possesses the secret key and generates MACs for messages of the attacker's choosing, the attacker cannot guess the MAC for other messages (which were not used to query the oracle) without performing infeasible amounts of computation.

    TODO: what security notion do MACs need to satisfy?

MAC vs digital signature

* MACs are symmetric - they are generated and verified using the same secret key
* Digital signatures are asymmetric - the signature is generated with the private key and verified with the public key
* => any user who can verify a MAC can also generate a MAC for other messages
* => MAC does not prove who sent the message, only that they had access to the secret key
* => MACs cannot provide "non-repudiation" out of the box (TODO: what is this exactly?)
    * can put key verification in a HSM which only verifies in theory means that holder of the HSM can only verify

MAC algorithms are built from other crypto primitives e.g.

* hash functions --> HMAC
* block cipher algorithms -->  OMAC, CCM, GCM, PMAC
* universal hashing --> UMAC-VMAC, Poly1305-AES
* ??? --> SipHash

> Additionally, the MAC algorithm can deliberately combine two or more cryptographic primitives, so as to maintain protection even if one of them is later found to be vulnerable. For instance, in Transport Layer Security (TLS), the input data is split in halves that are each processed with a different hashing primitive (SHA-1 and SHA-2) then XORed together to output the MAC.

