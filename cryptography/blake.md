# BLAKE

- a cryptographic hash function (a one-way function which maps an arbitrarily
  long input to a fixed size output (the "hash" or "message digest")
- announced in 2008 ???
- based on the ChaCha stream cipher but modifies it
- two variants based on word size:
    1. BLAKE-512 and BLAKE-384
        - use 64 bit words
        - produce digests of 512 and 384 bits respectively
    1. BLAKE-256 BLAKE-224
        - use 32 bit words
        - produce digests of 256 and 224 bits respectively
        - why the 224 bit digest size?
- was a contender to become SHA-3 but lost to Keccak
- Creators: Jean-Philippe Aumasson, Luca Henzen, Willi Meier, and Raphael C.-W.
  Phan

# BLAKE 2

- based on BLAKE
- is a family of algorithms with many variants
- announced 2012
- Creators: Jean-Philippe Aumasson, Samuel Neves, Zooko Wilcox-O'Hearn, and
  Christian Winnerlein
- variants
    1. BLAKE2b (successor to BLAKE-512)
        - RFC 7693
    1. BLAKE2s (successor to BLAKE-256)
        - RFC 7693
    1. BLAKE2bp (4 way parallel)
        - Not in RFC because it's an optional extra
    1. BLAKE2sp (8 way parallel)
        - Not in RFC because it's an optional extra
    1. BLAKE2X
        - Not in RFC because it's an optional extra
        - a family of _Extensible Output Functions_ (XOF)
        - not an instance of a hash function
        - e.g. BLAKE2Xb16MiB = BLAKE2X based on BLAKE2b producing 16 MiB digests
- usage
    - very popular
    - Argon2 uses BLAKE2b
    - Wireguard uses BLAKE2s
    - Linux kernel uses BLAKE2s for hashing the entropy pool in the random
      number generator
- implementations
    - libsodium
    - openssl
    - others

# BLAKE 3

- https://github.com/BLAKE3-team/BLAKE3
- based on BLAKE 2
- announced 2020
- Creators: Jack O'Connor, Jean-Philippe Aumasson, Samuel Neves, and Zooko
  Wilcox-O'Hearn
- A single algorithm
- features:
    - Parallelism
        - has a merkle tree structure so supports practically unlimited
          parallelism (both SIMD and multithreading)
    - XOF (Extensible Output Function)
    - KDF (Key derivation function)
    - PRF (Pseudorandom function)
    - MAC (Message authentication code)
- has official implementations in Rust and C
- consistently a few times faster than BLAKE2

> merkle tree format allows for verified streaming and incremental updates TODO:
> explain.

Good talk: Too much crypto: https://youtu.be/vDQRLXP62p8 \* an argument for not
making crypto primitives too secure (and therefore too slow)
