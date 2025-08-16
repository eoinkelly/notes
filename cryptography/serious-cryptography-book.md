# Serious Cryptography

## Chapter 1

Terms

- encryption is the principal application of cryptography
- make data incomprehensible to ensure it's confidentiality
- encryption has 2 parts:
    1. algorithm (aka cipher)
    2. secret key
- A cipher is
    - the algorithm used to get from plaintext to ciphertext and back again
    - is two functions (also called modes): encrypt and decrypt
    - confusingly, there is also a "mode of operation" for the algorithm which
      is unrelated

> ciphertext can be same length as plaintext or longer. It can never be shorter
> than the plaintext.

    ciphertext should be random enough to not compress
    plaintext messages of the same size should generate ciphertext of the same size - otherwise it is insecure
    all plaintexts of 1 kB should create ciphertexts of n kB
    https://stackoverflow.com/questions/62941859/can-a-cipher-be-shorter-than-the-original-text

    * TODO: why exactly?

### Categories of cipher

We can categorise ciphers in a few ways

- symmetric encryption
    - encrypting key and decrypting key are the same (or one can be easily
      derived from the other via a transformation)
    - note: this does not mean the encryption and decryption **ciphers** are
      exactly the same, only that they use the same key as input
- "input processing" type
    - block algorithm: complete blocks are encrypted
        - examples: AES, IDEA, Blowfish, DES, RC5, RC6
    - stream algorithm: data is encrypted as it arrives \* can operate on single
      byte or single bit
        - examples: RC4
- classical cipher
    - encryption and decryption can happen in your head or pen+paper

### Classical ciphers

#### Permutation

- Classical ciphers use substitution
- but it can't be just any substitution - it must be a **permutation** i.e. each
  substitution must have a **unique** inverse
    ```
    A,B,C => D,E,F # is a permutation
    A,B,C => D,D,E # is not a permutation
    ```
- not all permutations are secure!
- A secure permutation:
    1.  it should be determined by the key
    2.  different keys should result in different permutations
    3.  it should look random
        - there should be no pattern in the ciphertext after performing a
          permutation
        - patterns in ciphertext make a permutation more predictable, therefore
          less secure
- so a _secure permutation_ is necessary for a secure cipher but it's not
  sufficient.

#### Mode of operation

A cipher takes a chunk (could be a block or a byte) of plaintext and creates a
chunk of ciphertext (or vice versa)

Ciphers need a _mode of operation_ (or _mode_) to support messages of any
length.

You can have a secure permutation but use it in an insecure way which reveals
patterns in the ciphertext i.e. encrypting an individual letter/block/byte is
secure but wholistically the system is not secure because of failures in the
_mode_.

Things in the domain of the mode:

- how long is the key
- is the key reused for multiple messages

#### Example: Caesar cipher

- categories
    - substitution cipher
    - classical cipher
    - symmetric
- an alphabetic shift 3 positions to right
- no secret involved (shift is always 3)
- key is fixed and known but different key required to decrypt (left shift 3
  positions)
    - is caesar symmetric?
        - yes case: you can easily derive the decryption key from the encryption
          key
        - no case: the encryption key and decryption keys are technically
          different
- even making the shift amount secret isn't secure because it's trivial to try
  all 25 possible shifts

#### Example: Vigenere Cipher

- categories
    - substitution cipher
    - classical cipher
    - symmetric
- Invented in 16th century by italian Giovan Battista Bellaso
- Attributed to frenchman Blasie de Vigenere who invented a different cipher
- Used in american civil war and WW1
- Similar to Caesar cipher but shifts are based on a key
- Key is a collection of letters which represent numbers based on their position
  in the alphabet relative to A e.g. D => 3 because it is 3 after A
- The key is repeated as many times as necessary to encrypt the plain-text
- Security
    - more secure than Caesar
    - still easy to break
    - breaker needs to figure out key length first
    - can do so by looking for repeated sequences in the ciphertext
    - or via frequency analysis (using the known likelihood of letters to appear
      in language)
    - can be ok if the message is short compared to key length or if secrecy
      only required for a short period of time.

#### Example: random ideas

- letter based ideas for substitution ciphers:
    - idea: map a-z to a set of 26 emoji
        - equivalent secrecy to caesar I guess?
        - it's a fixed shift from plain text to cipher text
        - maybe bit better than caesar because you have more than 25 shifts
          available but still vulnerable to frequency analysis and guessing
          patterns
    - idea: map a-z to 26 numbers
        - it's a fixed shift from plain text to cipher text
            - can be any numbers but need to be able to tell where they end e.g.
              comma separated
        - equivalent secrecy to caesar I guess?
- bit based ideas for substitution ciphers
    - ???

#### Example Perfect Encryption - the one-time pad

- categories
    - classical cipher
    - symmetric
    - substitution
- ++ is perfectly secure provided the key is at least as long as the message and
  never re-used
- ++ simple to implement
- -- is very impractical in most cases
- each key must only be used once (it's in the name)
- proved secure by Claude Shannon
    - Assume K is random
    - then C looks as random as K because XOR of a random string with anything
      is random
    - if C is totally random then then all possible plaintexts are equally
      likely so the attacker has to try them all
    - therefore it is secure
    - of course that doesn't really matter if the attacker has the resources to
      try all the plaintexts
        - in that case you have to hope your set of possible plaintexts is big
          enough to make that infeasible
    - example: cipher text 128 bits long with key 128 bits long
        - => $2^{128}$ possible ciphertexts
        - => $2^{128}$ possible plaintexts provided the key is also 128 bits
          long
    - example: cipher text 128 bits long with key 64 bits long
        - => $2^{64}$ possible ciphertexts
        - => $2^{64}$ possible plaintexts
        - i.e. the attacker can rule out the majority of possible 128bit strings
            - TODO: I get this in probability terms but not sure how it works
              out practically
            - I guess this is showing that it is less work to break the cipher,
              not saying **how** you would rule those out?

In terms of letters

- a stream of shift indexes chosen at random
- the stream is at least as long as the message you want to send
- like a vigenere but no repeating!

In terms of bits

- uses the same algorithm for both encryption and decryption (`xor` with the
  key)
- The plain text, cipher text and key all treated as **bit** streams

Why does XOR work?

    We define C as:
    C = P XOR K

    And want to prove that

    P = C XOR K
      = (P XOR K) XOR K
      = P XOR K XOR K       # order of eval doesn't matter because XOR is associative
      = P                   # because K XOR K = all 0 bits and we know P XOR 0 = P

### Aside: permutations

Permutation

- is an action on a ordered set
- a permutation arranges it's members into a new sequence

Permutation is not a _combination_ which are selections of a set which ignore
order

Both permutations and combinations can work on subsets of the set but we ignore
that here

Given a set:

    1,2,3

then the permutations are

    1,2,3
    1,3,2
    2,3,1
    2,1,3
    3,1,2
    3,2,1

and the combinations are

combinations taking 3 at a time

    1,2,3

combinations taking 2 at a time

    1,2
    2,3
    1,3

combinations taking 1 at a time

    1
    2
    3

The number of permutations of $n$ distinct objects is $n!$

Define n factorial ($n!$) as:

$$n! = n \times (n - 1) \times (n - 2) \times ... \times 3 \times 2 \times 1$$

where we define:

$$1! = 1$$ $$0! = 1$$

This mathematical construction gives us the number of permutations in a set(?)

Example: the alphabet a-z (26 letters, a set with 26 elements) Pick a letter.
Then there are 25 choices for the next letter and 24 for the letter after that
and so on

Note it does not distinguish between secure and insecure permutations

### "perfect secrecy"

The goal of encryption is to prevent an attacker from finding out anything about
a message other than it's length. i.e. the attacker can't rule out any possible
plaintext given some ciphertext. The attacker must treat all plaintexts as
equally likely.

### Probability in Cryptography

- 1 = event definitely happens
- 0 = event never happens

We use probability to measure an attack's chances of success:

$$P_\text{succeed} = \frac{\text{num successful events}}{\text{num possible events}}$$

Imagine a situation where we have a **single** secret key $n$ bits long:

$$P_\text{succeed} = \frac{1}{2^n}$$

Which means that the probability of **failing** to find the key is:

$$P_\text{fail} = 1 - \frac{1}{2^n}$$

## Types of security

    IND-CPA secure

We need words to describe the security properties of algorithms. We use:

1. Attack models
1. Security goals

We say a cipher achieves a particular security notion if any attacker working in
a given model cannot achieve the security goal

    Security notion = Attack model + Security goal

### Attack models

An attack model is assumptions about:

1. how attackers might interact with a cipher
2. what they can and can't do

Used by

- setting requirements for cryptographers designing ciphers
- give guidelines to users about when a cipher is safe to use
- give cryptanalysts clues about how to break ciphers

They are models not reality. It is better to overestimate what an attacker can
do than underestimate.

> As the statistician George E. P. Box put it, "all models are wrong; the
> practical question is how wrong do they have to be to not be useful."

All models assume that the security of the cipher should rely only on the
secrecy fo the key and not the secrecy of the cipher. This is Kerckhoff's
principle, coined during a time when the "cipher" was a mechanical box that you
wanted to keep out of enemy hands.

Types of model

#### Black-box model

Query = send input to a function and see the output but nothing about how the
function works Encryption query = send plaintext into the encryption function
and see the ciphertext output Decryption query = send ciphertext into the
encryption function and see the plaintext output \* e.g. smart card chip where
you can connect and try to decrypt any ciphertext you choose

? Black box models (from weakest to strongest)

1. Ciphertext-only attackers (COA) (weakest attacker)
    - totally passive attacker
    - can observe ciphertext but don't know anything about plaintexts
2. Known-plaintext attackers (KPA)
    - passive attacker
    - can observe ciphertext but also have access to plaintexts i.e. they have a
      set of (plaintext, ciphertext) pairs
    - they cannot create their own plaintexts - they are still passive
3. Chosen-plaintext attackers (CPA)
    - active attackers, they can influence encryption
    - can choose plaintexts and observe the resulting ciphertexts
    - can perform encryption queries
    - this is the default model for asymmetric encryption
4. Chosen-ciphertext attackers (CCA) (strongest attacker)
    - active attackers
    - can choose plaintexts and observe the resulting ciphertexts
    - can perform encryption queries and decryption queries
    - e.g. breaking the decryption key in a chip in all playstation's or
      whatever. You can encrypt and decrypt but what you want is the key

#### Grey-box model

- attacker has access to the cipher's implementation
- more realistic than black box
- these models are harder to define than black-box
- attacks often depend on side-channels or other analog/real world properties of
  the system rather than just the algorithms's inputs and outputs
    - => crypto theory will often fail to model these properly
- examples

Example grey-box attacks

1. Side-channel attack
    - side-channel = a source of information that depends on the implementation
      be in hardware or software
    - software e.g.
        - execution time
        - error messages
        - return values
        - branches
    - hardware e.g.
        - power consumption
        - acoustic noise
        - electromagnetic emanations
2. Invasive attack
    - e.g. rip the top of the chip and have a look

### Security goals

1. Indistinguishability (IND)
    - ciphertext should be indistinguishable from random strings
    - game
        1. attacker picks plaintexts $P_1$ and $P_2$
        1. you encrypt one of them and give back to attacker
        1. attacker should not be able to tell which plaintext was encrypted
            - even if the attacker has access to the encryption function and can
              encrypt both $P_1$ and $P_2$ themselves!
                - this implies that the same plaintext should encrypt to
                  different ciphertext every time
                - what common encryptions do this? all of them? some?
2. Non-malleability (NM)
    - Given a ciphertext $C_1$ it should be impossible to create another
      ciphertext $C_2$ whose corresponding plaintext $P_2$ is related to $P_1$
      in any meaningful way
    - Aside: the one-time pad is malleable.
        - what are the consequences of this?

### Security notion

Expressed as:

    GOAL-MODEL

examples:

    IND-CPA
    NM-CCA

More ways to slice encryption applications

1. In transit
2. At rest

The right security notion to apply depends somewhat on whether the data is in
transit or at rest.

#### IND-CPA

The most important notion is:

    IND-CPA also known as "semantic security"

which captures the intuition that a ciphertext shouldn't leak info about the
plaintext as long as the key is secret.

To achieve IND-CPA, the encryption function must return different ciphertext for
the same plaintext every time it is called.

DRNG

- A DRNG = a deterministic random number generator
- A DRNG returns random looking bits given some secret value

IND-CPA can be achieved by introducing some random bits to the encryption
process.

Terminology:

- $K || R$ denotes $K$ followed by $R$
- $R$ = a random bit string chosen for each encryption

$$C = E(K,R,P)$$

If we let

$$X = DRBG(K || R) \otimes P$$

Then

$$E(K, R, P) = (X, R)$$

This randomization makes the ciphertexts longer than the plaintexts.

#### Relationships between security notions

- IND-CCA implies IND-CPA
- NM-CCA implies NM-CPA
- IND-CPA does **not** imply NM-CPA
- NM-CPA implies IND-CPA

### Asymmetric encryption

- public and private key
- **the public key is computed from the private key**
- usually combined with symmetric encryption to create a system

### Ciphers with extras

- Authenticated encryption (AE)
    - returns an authentication tag was well as the ciphertext
    - authentication stage = short sting that's impossible to guess without the
      key
    - encryption: $AE(K, P) = (C, T)$
    - decryption: $AD(K, C, T) = P$ but it also verifies that T is valid
    - the tag ensures the integrity of the message and is evidence that the
      ciphertext received is actually the one created by the owner of the key
      $K$
    - if you can trust that only the expected sender has $K$ then it also
      authenticates the message
    - is vulnerable to replay attack if a MITM can record some messages and
      later send them again
        - can mitigate by adding a counter to the message so receiving software
          would reject any message it had seen before
    - Authenticated encryption with associated data (AEAD)
        - an extension of AE
        - calculates the tag over the ciphertext and some cleartext data.
        - uses:
            - protect protocol's datagrams with a cleartext header and encrypted
              payload
- Formate preserving encryption
    - used when you want the ciphertext to have the same format as the plaintext
      e.g. an IP address, ZIP codes, credit card numbers
    - might want same format because storing in a legacy system.
- Fully homomorphic encryption (FHE)
    - the "holy grail"
    - allows you to apply edit an encrypted document without decrypting it
    - first FHE scheme created in 2009 but it's still too slow to be usable
- Searchable encryption
    - allows searching the encrypted database with an encrypted query so the
      database owner never knows the query
    - currently experimental, could be very useful in cloud contexts
- Tweakable encryption
    - has an extra "tweak" input
    - the tweak can be unique per customer
    - main application is on-disk encryption
        - uses a tweak based on the position of the data encrypted (sector and
          block index)

### Ways things can go wrong

1. weak cipher
    - cipher is easier to break than you assumed
2. wrong model
    - you implement a good cipher but make the wrong assumptions about your
      attackers
    - or you don't think about some side-channel

## Chapter 2: Randomness

- "random bits" actually means "randomly generated bits"
    - the bits are not random, the algorithm that generates the bits is random
- humans are bad at knowing the difference between "actually random" and "random
  looking"
    - we think an object was randomly generated if it looks random
    - we think that patterns appearing by chance are there for a reason other
      than chance
- the randomness of a process is described by it's probability distribution
    - the

If there are $N$ possible events

Then there are $N$ probabilities $p_1, p_2, ... p_n$

and the sum of all the probabilities is 1

$$p_1 + p_2 + ... + p_n = 1$$

A _uniform distribution_ occurs when all probabilities on the distribution are
equal i.e. they are all equally likely to occur

$$p_1 = p_2 = ... = p_n$$

For $N$ events, each event has a probability of

$$p_n = \frac{1}{N}$$

A _non-uniform_ distribution is when all probabilities are not equal - the
system is biased towards some events.

### Entropy

- a measure of the surprise/uncertainty/disorder in a system
- a measure of the amount of surprise found in the result of a randomized
  process
- see [entropy notes](./entropy.md) for details
- Eta (greek letter H) is the symbol for entropy

$$\Eta = \sum_{i=1}^{N} p_i \times log_2(\frac{1}{p_i})$$

which can be refactored to:

$$\Eta = - \sum_{i=1}^{N} p_i \times log_2(p_i)$$

Binary log expresses the information in bits and yields integer values when
probabilities are powers of two

$$log(\frac{1}{2}) = -1$$ $$log(\frac{1}{4}) = -2$$ $$log(\frac{1}{2^n}) = -n$$

- The entropy of a **uniformly distributed** n bit (binary digit) string is n
  bits (shannon bits) of entropy.
- Entropy is maximised when the distribution is uniform because a uniform
  distribution maximises uncertainty/surprise.
- When the distribution is not uniform then the entropy is lower.
- lower entropy values imply less randomness
- Entropy is also a measure of information You get the most information when you
  learn the most i.e remove the most uncertainty

### Types of RNG

- RNGs
    - usually harvest entropy from the environment:
        - temperature measurements
        - acoustic noise
        - air turbulence
        - electrical static
        - Disk I/O
        - network activity
        - running processes
        - key presses by human
        - mouse movements
    - such sources are
        - not always available
        - it can be hard to measure their actual entropy
        - don't produce bits very quickly
- QRNG (Quantum RNG)
    - get randomness form quantum mechanical phenomena:
        - radioactive decay
        - vacuum fluctuations
        - photon polarization
    - downsides
        - can be biased
        - don't produce bits very quickly
        - needs specialized hardware
- PRNGs (Pseudo random number generators)
    - take in small number of real random bits
    - use an algorithm to generate a larger number of random looking bits
    - ++ they work quickly
    - ++ they can use digital sources
    - -- they work deterministically
    - Real RNGs real randomly generated bits. PRNGs produce random looking bits
    - PRNGs are kind of like a randomness amplifier
    - they use the actual random bits and put them into a _Deterministic random
      bit generator (DRBG)_
    - need to ensure the DRBG never gets the same input twice

### Operations within a PRNG

1. `init()`
    - initializes the entropy pool to some default value
    - initializes the internal state of the PRNG variables and buffers
1. `refresh(R)`
    - updates the entropy pool with some data `R`, usually sourced from an
      actual RNG
    - if there are not sufficient random bits available from the RNG,
      deterministic values may be used
    - sometimes called "reseeding" with the "seed" `R`
    - usually called by the operating system
1. `next(N)`
    - return the next N pseudorandom bits **and** updates the entropy pool
    - runs the DRNG
    - modifies the entropy pool to ensure the next call will yield different
      random bits
    - usually called by the application requiring random bits

### PRNG desirable security features

A PRNG should be

1. resistant to backtracking (forward secrecy)
    - previously generated bits are impossible to recover
    - if an attacker can get access to the current value of the entropy pool,
      they should not be able to use it to work out previous values of the pool
      or previously generated bits
    - `next()` and `refresh(R)` operations should be irreversible
2. resistant to prediction (backward secrecy)
    - future bits should be impossible to predict
    - if an attacker can get access to the current value of the entropy pool,
      they should not be able to use it predict future values of the pool or
      future generated bits
    - `refresh(R)` should be called regularly with `R` values that are unknown
      to the attacker and difficult to guess
    - even if the `R` values are known, attacker would need to know the order
      that `refresh(R)` and `next()` operations were called to reconstruct the
      pool.
3. Author says this is not a complete list ...

### PRNG implementations

### Yarrow

- 1998 by Kelsey a& Schneier
- used in macOS and iOS

### Fortuna

- 2003 by Ferguson and Schneier
- replaced yarrow in Windows

Fortuna implementation

- 32 entropy pools $P_1, P_2,...,P_32$ such that $P_i$ is used every $2^{32}$
  reseeds.
- Internal state of the DRBG
    - A 16 byte key K
    - A 16 byte counter C

- `init()`
    - sets K and C to 0 and empties all the entropy pools
- `refresh(R)` appends R to one of the entropy pools
    - the OS chooses the RNGs used to produce R values
    - the OS calls `refresh(R)` regularly
- `next()`
    - updates K using data from one or more of the entropy pools
    - pool choice depends mainly on how many updates have already been done to K
    - The N requested bits are then produced by encrypting C using K as a key.
        - if more bits are required, fortuna will encrypt C+1, C+2 etc. to
          generate enough bits
    - there is a cipher baked into `next()`
    - spec defines how the cipher should work but doesn't include a test suite
    - edge-cases to handle
        - what to do if you run out of entropy from the RNG

Ways it can go wrong

- You implement the algorithm incorrectly
- Multiple instances of fortuna share a seed file (seed files must never be
  re-used)
- you have just rebooted so not enough RNG data is available yet

### Cryptographic PRNG vs Non-Cryptographic PRNG

- Non-crypto PRNGs
    - produce uniform distributions for simulations and video games but are not
      secure enough to use for crypto
    - They care about the quality of the bits probability distribution and not
      about their predictability
    - Examples
        - `rand` and `drand48` in libc
            - The rand48() family of functions generates pseudo-random numbers,
              using a linear congruential algorithm working on integers 48 bits
              in size.
        - `drand48` in libc
        - `rand` in PHP
        - `Random` in Ruby
        - `random` in Python
- Crypto PRNGs (CPRNG)
    - Are unpredictable as well as delivering a uniform distribution of
      probabilities

### Non-crypto PRNG example: Mersenne Twister

- Used by PHP, Ruby, Python, R etc.
    - From ruby `Random` docs
        > PRNGs are currently implemented as a modified Mersenne Twister with a
        > period of 2\*\*19937-1.
- generates uniformly distributed random bits without statistical bias
- but it's predictable - given a few bits produced by MT, it easy to tell which
  bits will follow

### Aside: Ruby SecureRandom

- Supports the following secure random number generators:
    - openssl
    - /dev/urandom
    - Win32
- TODO: the docs don't say how it picks or how it works under the hood
- `openssl rand <num bytes> -out <output file>` get random data out of openssl

### Security: linearity (insecure) vs non-linearity (secure)

Mersenne twister is weak because it is linear.

XOR is a linear combination. Consider three bits: X, Y, Z

$$output = X \otimes Y \otimes Z$$

If you flip a bit in X then the output changes no matter what the value of Y and
Z is.

Linear combinations are predictable.

> You don't need to know the value of the bits in order to predict how a change
> in their value will affect the output

Consider

$$output = X \wedge Y \otimes Z$$

This is not a linear combination because changing X will not **always** change
the output.

Linear transformations lead to short equations which are easy to solve Nonlinear
transformations lead to equations of exponential size which are practically
unsolvable

Cryptographers design PRNGs so that they emulate complex non-linear
transformations with just a small number of simple operations

    TODO: I don't fully understand linearity

### Statistical tests are not a measure of cryptographic security

There are standard test suites for PRNGs

- TestU01
- Diehard
- NIST test suite

They test the output against a perfect uniform distribution, check distribution
of 0 and 1 bits or the distribution of 8-bit patterns. These tests are **not a
measure of PRNG security**

### Real-world PRNG user interfaces

- Linux
    - `/dev/urandom`
        - `/dev/urandom` is the userland interface to the crypto PRNG in the
          kernel
        - `dd if=/dev/urandom of=myfile bs=1M count=10`
        - when you read it in C, you **must check the result of open() and
          read()** lest your buffer be unchanged or just filled with 0's
        - based on Blake2s now
          https://lore.kernel.org/lkml/20211223141113.1240679-2-Jason@zx2c4.com/
    - `/dev/random`
        - contains an entropy estimator
            - these are considered unreliable and can be fooled by attackers
            - current entropy estimate in
              `/proc/sys/kernel/random/entropy_avail`
        - will refuse to return bits if the entropy is too low so can cause a
          DoS
        - in practice it's no more secure than `dev/urandom` and creates more
          problems than it solves
    - `getrandom()` (linux)
        - will block if it hasn't gathered enough entropy during `init()`
        - won't block after that
- Windows
    - `CryptGenRandom()`
        - legacy userland interface to kernel PRNG
    - `BcryptGenRandom()`
        - more modern userland interface to kernel PRNG
- Hardware: `RDRAND` in Intel
    - Access via `RDRAND` assembly instruction
    - based on NIST SP-800-90 guidelines
        - uses AES in CTR_DRBG mode
    - entropy source is a _dual differential jamb latch with feedback_
    - a small hardware circuit which jumps between 0 and 1 at 800 MHz depending
      on thermal noise fluctuation
    - only partially documented
    - some security concerns about it being backdoored post snowden
        - can be mitigated by mixing the bits with other sources
    - Intel also provides `RDSEED` instruction which returns some random bits
      directly from the entropy source
        - This is designed to be used to seed other PRNGs
    - Json Donenfeld says `RDRAND`is actually slow
      https://lore.kernel.org/lkml/20211230165052.2698-1-Jason@zx2c4.com/

### Ways PRNG can go wrong

1. Choose a poor entropy source
    - choosing guessable sources like
        - time of day
        - PIDs of processes
2. Insufficient entropy at boot time
    - At boot-time, if there is not yet sufficient entropy, the PRNG might
      fallback to hard-coded values
    - 2012 issue where SSH servers were generating the same keys
3. Using a non-cryptographic PRNG
    - MediaWiki used Mersenne Twister to generate security tokens and temp
      passwords in the past
4. Sampling bugs
    - cryptocat (now discontinued secure chat app) issue:
        - Generating a set of random digits in range 0-9
            - You start with a stream of hopefully cryptographically secure
              bytes (0 -> 255)
            - Naive approach would be to just modulo 10 but this won't give an
              uniform distribution of outputs
            - You need to ignore any values from the PRNG above value 249 to
              ensure you get an even spread of outputs
            - It's 249 not 250 because we assume 0 indexed array. `cryptocat`
              got this wrong in an old version
