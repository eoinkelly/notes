# PRF Pseudorandom function family

https://en.wikipedia.org/wiki/Pseudorandom_function_family

> a pseudorandom function family, abbreviated PRF, is a collection of efficiently-computable functions which emulate a random oracle in the following way: no efficient algorithm can distinguish (with significant advantage) between a function chosen randomly from the PRF family and a random oracle (a function whose outputs are fixed completely at random). Pseudorandom functions are vital tools in the construction of cryptographic primitives, especially secure encryption schemes.

> Pseudorandom functions are not to be confused with pseudorandom generators (PRGs). The guarantee of a PRG is that a single output appears random if the input was chosen at random. On the other hand, the guarantee of a PRF is that all its outputs appear random, regardless of how the corresponding inputs were chosen, as long as the function was drawn at random from the PRF family.
