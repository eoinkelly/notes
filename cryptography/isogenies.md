# Post quantum ECC with isogenies

https://www.youtube.com/watch?v=9B7jq7Mgiwc

* SIDH = Supersingular isogeny Diffe-Hellman key exchange
* Quantum computers are good at solving certain types of problem
* One of the types is:
    * discrete logarithm problems in a finite field
    * discrete logarithm problems in an elliptic curve
* Unfortunately current public-key crypto is based on these problems
* So we want a problem which is hard to solve on classical computers **and** quantum computers

Aside: Will quantum computers threaten symmetric crypto too?

* Elliptic curves are the simplest geometric object that offers security similar to the finite-field discrete log problem
* Elliptic curves are both algebraic and geometric objects
    * from a crypto standpoint, you can ignore the geometry and just use the algebra

## Mathematical terms

### Finite field

### Group

### Sub-group

### Isomorphisms

Isomorphisms are a mapping from one EC curve to another EC curve
are a special case of isogenies



### Kernel

"The kernel of an isogeny"
"The kernel of an isomorphism"
it contains points on the curve

### Velu's formulas