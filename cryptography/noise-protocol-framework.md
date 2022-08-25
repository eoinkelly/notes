# Noise protocol framework

Used by

* Whatsapp use it instead of TLS
* Between phone and laptop when doing platform independent webauthn (CTAP2 runs over a noise tunnel)
* Wireguard
* Lightning (layer-2 thing on top of Bitcoin)

## Handshake pattern naming convention

* different patterns defined in the spec with their security properties well defined
* examples
  * Noise_NN()
    * not authentiated on servier side (first N) or receive side (second N)
  * Noise_XX()

## Overview

* Deals with 2-party, online protocols
* Author wanted a simpler way to build 2-party, online, secure-channel protocols for needs not met by TLS/SSH etc.
* Limits the primitives you can use to keep things simple
* A _framework_ for building crypto protocols
	* I think this means that it sets up an encrypted tunnel but doesn't specify what goes in it
* You can build complex protocols by combining mulitple noise protocols
* Based on Diffe-Hellman key agreement
* each party has long-term static key-pair AND/OR an ephemeral key-pair

## Terms

* Tokens
  * basic units of the language
* _Message pattern_
  * A sequence of tokens that specifies the DH public keys that make the handshake **and** the DH functions which are performed when sending/receiving the message
* _Handshake pattern_
  * A sequence of messages that comprise a handshake
* _Concrete noise protocol_
	* a particular instance of a handshake pattern

## Phases

1. Handshake
	* parties exchange DH public keys and perform a sequence of DH operations
	* hash the DH results into a **shared secret key**.
1. "Normal" phase (not sure of name)
	* Use the shared secret key from the handshake to exchange encrypted _transport messages_