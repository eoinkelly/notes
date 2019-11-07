# CJ Date: An introduction to set theory video 1

Sources

* http://player.oreilly.com/videos/9781491908778?toc_id=182148

The _relational theory_ is heavily based on classical set theory. Set theory
has serveral parallels with _logic_

Definition of a **Set**

1. A collection S of distinct _elements_ or _members_ such that given an
   arbitrary object X it can be determined whether X appears in S.
2. Sets do not contain duplicate elements!
3. Sets do not have an ordering
4. Elements can be absolutely anything - even other sets
5. A set can have 0 elements and still be a valid set

Standard representation of a set (surrounded by curly braces, elements
separated by commas). This is often known as a "commalist":

$$\{ 1, 3, 4, 89 \}$$

* Below is the same set as the one above!
* It gives us no extra information over the set above!
* Technically this is a "bag" not a set but if we consider it as a
  representation of a set then it is the same as the one above.

$$\{ 1, 1, 1, 3, 3, 4, 89 \}$$

This is the same set as above (ordering does not matter):

$$\{ 3, 1, 4, 89 \}$$

* In mathematics the object that can contain duplicates is called a "bag" (or "multiset")
* Empty set notations: $\{\}$ or $\emptyset$ mean empty set

Ways of specificing a set:

1. Enumeration
    * basically write them down
    * only works if set is finite
    * only practical if set is small
2. Predicate (the defining characteristic of elements in the set)
    ```
    # these sets are the same
    { x : x is a prime less than 10 }
    { 2, 3, 5, 7 }
    ```
    * A predicate is a bit like a "logic test"
3. Substitution
    ```
    { x : x^2 is integer less than 100 }
    ```
    * Predicate is a special case of substituion


## Terminiology

* Cardinality
    * The number of elements in a given set is called _cardinality_
    * In set theory cardinality can be infinite but in computing our sets are finite
* Containment
    * A set is said to _contain_ its members
* Set membership
    * The property of appearing in some set
        OR
    * The operation  (denoted by ∈) of testing for that property
        so ∈ and ∉ are function/operator names!

* x ∈ S returns true iff x is a member of S

```
5 ∈ { 2, 3, 5, 7 } : TRUE
8 ∈ { 2, 3, 5, 7 } : FALSE
```

$x \notin S$ is defined as $NOT(x \in S)$ aka $\lnot(x \in S)$

Note there is a logical difference between $x$ and a set that contains just $x$. $\{x\}$  is the so called "singleton set".

$$5 \neq \{5\}$$

CJD says that SQL gets into trouble here with subqueries because sometimes they
represent the element itself and sometimes they represent a set containing
just that element.

## Set theory

* A branch of mathematics
* closely related to logic
* formalizes the concept of a set in terms of certian axioms
    * axiom of extension: S1 and S1 are equal (shorthand S1 = S2) iff they have
      the same members

Set Theory is built on top of the idea of "equality" and "equality
comparators". It needs the equality tests:

* Are two sets equal?
* Are two elements equal?

### Aside: $\equiv$ (triple bar)

Triple bar has a number of context dependant meanings in maths

* used in equations where the LHS is being defined not derived
* used for identical equality of functions e.g. $f \equiv g$ if $f(x) = g(x)$ for all $x$
* can mean "if and only if" in logic ($\Leftrightarrow$ is also used for this)

### Aside: axiom

An axiom or postulate is a premise or starting point of reasoning. As
classically conceived, an axiom is a premise _so evident as to be accepted as
true without controversy_.

A venn diagram is representing a set on paper as a circle or rectangle

### Defining terms: subset and superset

Note we are defining these, not deriving them

* S2 is a subset of S1 (S2 subsetof S1) iff every element of S2 is also an
  element of S1
* S1 is a superset of S2 (S1 supersetof S2) iff every element of S2 is an
  element of S1

* Every superset is a set
* Every subset is a set
    * $\implies$ every operation we can perform on a set we can perform on subsets and supersets
    * math takes nothing for granted!

If $S1 = S2$ then they are both supersets and subsets of each other i.e.

* S1 is a subset of S1
* S1 is a superset of S1

Note: "subset" and "superset" do not mean "bigger than" or "smaller than" as they are used colloquially - see "proper subset" and "proper superset" for that.

## Define term: Universal set

Universal set:
* The set U that contains all terms of interest
* We need this concept to put bounds on what a legal set and in particular what
  a legal superset is.
* It is like the superest of supersets
* Examples:
    * U is the set of all Intergers
    * U is the set of all employees

In logic, this concept is called the _domain of discourse_

## Define: inclusion

S1 _includes_ S2 iff S1 is a superset of S2
    S1 ⊇ S2
S2 is _included in_ S1 if it is a subset of S1
    S2 ⊆ S1

Memonic idea: the operator kind of looks like a sideways "in"

Consequences:

1. Every set is included in itself
2. Every set is a subset of itself
3. Every set is a superset of itself
4. Every set includes itself
5. Every set includes the empty set
6. The empty set is a subset of every set
7. Every set is a superset of the empty set
8. The universal set is a superset of every set
9. Every set is a subset of the universal set

We can now define "equality" between S1 and S1 by saying that "S1 and S2 are
equal iff S1 is a subset of S2 and S2 is a subset of S1" i.e. they include each
other.

Don't confuse _set inclusion_ ⊆ with _set membership_ ∈. A set _contains_ its
elements but _includes_ its subsets. Inclusion is an operator for comparing two
sets whereas membership is an operator for comparing an element and a set.

The set inclusion and set membership operators take different types of operand but it's subtle because and ELEMENT can also be a SET in this context

$$SET \subseteq SET$$
$$ELEMENT \in SET$$

## Power sets

The set of all subsets of S is the _powerset of S_ denoted P(S)

Given set
$$\{ 2, 3, 5 \}$$

then the _power set_ $P(S)$ is

$$\{ \{\}, \{2\}, \{3\}, \{5\}, \{2,3\}, \{2,5\}, \{3,5\}, \{2,3,5\} \}$$

and the cardinality of the _power set_ $n = 8$

If $S$ has cardinality $n$ then $P(S)$ has cardinality $2^n$ because each element has two possible states it can have:

1. it is in the set
2. it is not in the set

Notice that "all the subsets of a set" includes both the empty set and the set itself.

The power set represents all the possible combinations of a set of elements

Q: Is there an S where P(S) = S
A: In video he says no because cardinalities.

    n = cardinality(S)
    => cardinality(P(S)) = 2^n

    and n can never equal 2^n

    but what about the empty set?

    What is the cardinality of the empty set?
    0 I presume
    2^0 = 1
    P(S) is the set of all **subsets** of S which includes S itself so P({}) = {{}}


## Define: proper subset

* S1 is a _proper subset_ of S2 iff $S1 \subseteq S2$ and $S1 \neq S2$
* _proper subset_ uses the notation: $S1 \subset S2$
* The memonic for the symbol is that it looks a bit like a _greater than or equal to_ i.e. the pointy end points towards the smaller thing
* The latex symbol reads from left to right

## Define: proper superset

* S1 is a _proper superset_ of S2 iff $S1 \supseteq S2$ and $S1 \neq S2$
* _proper supset_ uses the notation $S1 \supset S2$

## Define: proper inclusion

* $S1$ _properly includes_ $S2$ if $S1 \supset S2$
* $S2$ is _properly included_ in $S2$ if \\(S2 \subset S1 $\\)

Aside: the terms _subset_ and _superset_ are often colloquially used to refer to _proper subset_ and _proper superset_ e.g. Unicode standard, Latex symbol memonics etc.

## Define: Union

$$A \cup B \equiv \{x : x \in A \quad \text{OR} \quad x \in B \}$$

_A UNION B is defined as the set of objects x such that x is an element of A or x is an element of B_

## Define: Intersection
$$A \cap  B \equiv \{x : x \in A \quad \text{AND} \quad x \in B \}$$

Also
$$A \cap B = A - (A - B)$$
$$A \cap B = B - (B - A)$$

_A INTERSECTION B is defined as the set of objects x such that x is an element of A and x is an element of B_

## Define: Minus

$$A -  B \equiv \{x : x \in A \quad \text{AND} \quad x \notin B \}$$

A MINUS B is defined as the set of objects x where x is an element of A but is not an element of B

$$B -  A \equiv \{x : x \in B \quad \text{AND} \quad x \notin A \}$$

Notice that $A - B$ is not the same as $B - A$

## Define: complement

Set theory and logic have an obvious relationship. The set theory version of logical NOT is _COMPLEMENT_.

| Set theory | Logic |
| -- | -- |
|UNION | OR |
|INTERSECTION | AND|
|COMPLEMENT | NOT |
|XUNION | XOR |

$$COMP(A) \equiv \{x : x \notin A \}$$
$$COMP(A) \equiv U - A$$

The universal COMPLEMENT of A is every element not in A i.e. "the Universe MINUS A"

U = the universal set


There is also the idea of a "relative complement" $B - A$ which is all the elements in B which are not in A

## Define Exclusive union

The set equivalent of XOR. This has no well accepted symbol

$$A \ XUNION \  B \equiv \{x : x \in A \  XOR \  x \in B \}$$

Also

$$A \ XUNION \  B = (A - B) \cup (B - A)$$

> There are exactly 4 "basic" monadic set operation and 16 "basic" dyadic set operators

Why?

In logic there are exactly 4 monadic connectors and exactly 16 dyadic connectors


## Set operator identities

### _commutativity_ is a property of *dyadic* operators where

    A OP B = B OP A

Set operators which are communative

* UNION: $A \cup B = B \cup A$
* INTERSECTION: $A \cap B = B \cap A$
* XUNION: $A \quad XUNION \quad B = B \quad XUNION \quad A$

The following operators are not commutative

* MINUS

### _Associativity_ is a property if _dyadic_ operators where

    A OP (B OP C) = (A OP B) OP C

Set operators which are assocative

* UNION: $A \cup B = B \cup A$
* INTERSECTION: $A \cap B = B \cap A$
* XUNION: $A \quad XUNION \quad B = B \quad XUNION \quad A$

The following operators are not associative

* MINUS

In SQL the `UNION` is not commutative - CJD thinks that it is probably associative but isn't sure

String concatenation is an operator which is associative but not commutative

Because UNION, XUNION, INTERSECT are **both** associative and commutative we can define _n-adic_ versions

    # 0-adic (dyadic) verion
    UNION() = UNION() = {}
    INTERSECT() = INTERSECT() = Universal Set

    # 1-adic (dyadic) verion
    UNION(A1) = UNION(A1)

    # 2-adic (dyadic) verion
    UNION(A1, A2) = UNION(A2, A1)
    UNION(UNION(A1, A2), A3) = UNION(A1, UNION(A2, A3))

    # 3-adic
    UNION(A1, A2, A3) = UNION(A3, A2, A1) # or any ordering of A1, A2, A3
    UNION(UNION(A1, A2, A3), A4) = UNION(A1, UNION(A2, A3, A4)) # or any ordering of A1, A2, A3, A4

The empty set is the _identity value_ for UNION and XUNION

$$A \cup \{\} = A$$
$$A \; \textrm{XUNION} \; \{\} = A$$

The Universal set is the _identity value_ for INTERSECT

$$A \cap U = A$$

### Distributivity

A property of dyadic operators

    A OPX (B OPY C) = (A OPX B) OPY (A OPX C)

Example from arithmetic: $3*(7+2)=(3*7)+(3*2)$

We say that _multiply distributes over addition_


UNION distributes over INTERSECTION

$$A \cup (B \cap C) = (A \cup B) \cap (A \cup C)$$

INTERSECTION distributes over UNION



XUNION does not distribute over UNION or INTERSECT (I'm pretty sure)

$$A XUNION (B \cup C) \neq (A XUNION B) \cup (A XUNION C)$$
$$A XUNION (B \cap C) \neq (A XUNION B) \cap (A XUNION C)$$

### Aside: difference between an operator and a function

This seems to be an area of fuzzy terminology in Math

> Both operators and functions define a mapping between inputs and outputs. For example, the common addition function or operator maps the inputs 2 and 3 to the output 5 (2+3=5).
>
> A function is commonly given the restriction of mapping to exactly one output for each of it’s inputs. Thus, the square-root operator is not strictly a function since it maps it’s inputs, individual numbers, to a pair of outputs, the positive and negative square-root (e.g., the square-root of 4 is 2 and -2).

* A binary operation on a set S is a special kind of function (operations are functions with constraints applied)
    * it takes two elements of S as input and returns another element of S
    * it can have additional constraints like associativity and commutativity applied to it
* A function $f: A \arrow B$ has no constraint that the input and output have to be the same type
    * it makes no sense to talk about associvitiy and commutativity if the types aren't always the same


### Idempotence

$$A \;\text{OP}\; A = A$$
$$A \cup A = A$$
$$A \cap A = A$$

* UNION and INTERSECT are idempotent in set theory but **not in SQL!**

## Absorption identity

A OPX (A OPY B) = A
$$A \cup (A \cap B) = A$$
$$A \cap (A \cup B) = A$$

UNION absorbs INTERSECT and vice versa in set theory but **not in SQL!**


$$A \cup U = U$$
$$A \cap U = A$$
$$A \cap \emptyset = \emptyset$$
$$A \cup \emptyset = A$$

De Morgan's Laws

$$\neg(A \cup B) = (\neg A) \cap (\neg B)$$
$$\neg(A \cap B) = (\neg A) \cup (\neg B)$$

All of these identities are direct counterparts of tautologies in logic


Principle of duality

Let EXP be any set theory expression involving UNION, INTERSECT, COMP, U and {}
We can derive EXP' from EXP by
    1. replace all UNION with INTERSECT
    2. replace all occurances of U with {}

We say that:

    EXP' is the **dual** of EXP
    EXP is the **dual** of EXP'

In particular, the dual of any theorem is a theorem e.g. Demorgans laws are duals of each other. There are analogous ideas in logic

Set algerbra

Define the universal set $U = P(S)$ where $P(S)$ is the power set of some set $S$

The combination of

* U
* UNION on elements of U
* INTERSECT on elements of U
* COMP on elements of U
* set inclusion on elements of U

makes an **algebra** which has the following properites:

1. Closure - the output of any operation above is another set
2. Commutativivty
3. Assoscitivity of UNION, INTERSECT
4. Distributivity of UNION, INTERSECT over each other
5. Identity values ($\emptyset$ for UNION, Universal set $U$ for INTERSECT)
6. $A \cup U = U$
7. $A \cap \emptyset = \emptyset$
8. Involution: $\neg(\neg A) = A$
9. $\neg\emptyset = U$
10. $\neg U = \emptyset$
11. Complementarity: $A \cup (\neg A) = U$ and $A \cap (\neg A) = \emptyset$
12. If $A \cup B = U$ and $A \cap B = \emptyset$ then $B = \neg A$
12. If $A \cup B = A$ for all $A$ then $B = \emptyset$
13. If $A \cap B = A$ for all $A$ then $B = U$
14. Idempotentce of $\cup$, $\cap$
15. Absorption of $\cup$, $\cap$
16. De Morgan's laws
17. Consistency: The following statements are all equivalent:
    * $A \subseteq B$
    * $A \cup B = B$
    * $A \cap B = A$


Relational algebra is based on set algebra so a lot (but not all) of these aply to that algebra

The direct parallel between set theory and logic

Let $p(x)$ be a monadic predicate. Then we define the **extension** of $p$ as a set $X(p)$ containing all the elements $a$ such that $a$ satisfies $p$ i.e. such that $p(a) = \textrm{TRUE}$

Example

    p(x) = "x is prime and less than 10"
    X(p) = {2,3,5,7}

So every monadic logic predicate has a corresponding _extension_ set which contains all the values which make that predicate TRUE.

Let p and q be monadic predicates and $X$ be the extension set then:

$$X(p) \cup X(q) = X(p \;\textrm{OR}\; q)$$
$$X(p) \cap X(q) = X(p \;\textrm{AND}\; q)$$
$$\neg X(p) = X(\textrm{NOT}\; p)$$

So the definitoin of extension sets allows us to flip between logic and set theory. All monadic logic predicates can be expressed as a set and the operations we perform on those logic primitives have equivalent set operations

> In the relational world, every relation is basically the extension of a certain predicate

All predicates can be thought of as monadic

Example of a dyadic predicate:

    p(x, y) = "x and y prime and less than 10 and x < y"
    then
    X(p) = {<2,3>, <2,5>, <2,7>, <3,5>, <3,7>, <5,7>}

so we can regard p(x, y) as p(z) where

    z = ordered pair <x, y> where x and y prime and less than 10 and x < y


## Definition: ordered pair

This is a definition of ordered pair in terms of sets:

$$<x, y> \equiv \{\{x\}, \{x, y\} \}$$

* $\{x, y\}$ says what the elements of the ordered pair are;
* $\{x\}$ says $x$ is the _first_ element

$$<x_1,y_1> = <x_2, y_2> \;\textrm{iff}\; x_1 = x_2 \;\textrm{and}\; y_1 = y_2$$


Cartesian product

The cartesian product of sets $X$ and $Y$ **in that order** = set of all possible ordered pairs $<x,y>$ where $x \in X$, $y \in Y$

* cartesian product requires the notion of ordered pairs

Example:

    {1,3} CART_PROD {2,3,5} = {<1,2>, <1,3>, <1,5>, <3,2>, <3,3>, <3,5>}

This result is a **binary relation** (which is a set theory idea)

Note that in general

    X CART_PROD Y != Y CART_PROD X

What happens if X or Y is empty set?

A **binary relation** on sets X and Y (in that order) is a subset of the cartesian product of X and Y in that order.

Given

    X = {1,3}
    Y = {2,3,5}

Here are some relations for X and Y:

    {<1,2>, <3,3>, <3,5>}
    {<1,2>, <1,3>, <1,5>, <3,2>, <3,3>, <3,5>}
    {<1,5>}
    {}

Relations in the relational model are based on the same concept with two extensions:

1. allow n to be any number not just 2
2. don't refer to the relation in left-right order, we refer them by name instead

Aside: A _function_ is a binary relation in which no two elements have the same first component
