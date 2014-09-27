# CJ Date: An introduction to set theory video 1

* http://player.oreilly.com/videos/9781491908778?toc_id=182148


The _relational theory_ is heavily based on classical set theory

Set theory has serveral parallels with _logic_


Definition of a **Set**

1. A collection S of distinct _elements_ or _members_ such that given an
   arbitrary object X it can be determined whether X appears in S.
2. Sets do not contain duplicate elements!
3. Sets do not have an ordering
4. Elements can be absolutely anything - even other sets
5. A set can have 0 elements and still be a valid set

You can have an empty set - a collection with no elements -

```
# Standard representation of a set (surrounded by curly braces, elements
# separated by commas

# This is often known as a "commalist"

{ 1, 3, 4, 89 }

# This is the same set as the one above!
# It gives us no extra information over the set above!
# technically this is a "bag" not a set but if we consider it as a
# representation of a set then it is the same as the one above.
{ 1, 1, 1, 3, 3, 4, 89 }

# this is the same set as above (ordering does not matter)
{ 3, 1, 4, 89 }

# In mathematics the object that can contain duplicates is called a "bag"
    perhaps this is where chef got it?


# Empty set notations
{}
or the 0 with forward slash through it
```

Ways of specificing a set:

1. Enumeration
    * basically write them down
    * only works iff set is finite
    * only practical if set is small
2. Predicate (the defining characteristic of elements in the set)
    ```
    # these sets are the same
    { x : x is a prime less than 10 }
    { 2, 3, 5, 7 }
    ```
3. Substitution
    ```
    { x : x^2 is integer less than 100 }
    ```
    * Predicate is a special case of substituion


The number of elements in a given set is called _cardinality_

Terminiology

* Cardinality
    * In set theory cardinality can be infinite but in computing our sets are finite
* Containment
    * A set is said to _contain_ its members
* Set membership
    * The property of appearing in some set
        OR
    * The operation  (denoted by ∈) of testing for that property
        so ∈ and ∉ are function names or operator!

```
x ∈ S returns true iff x is a member of S

5 ∈ { 2, 3, 5, 7 } : TRUE
8 ∈ { 2, 3, 5, 7 } : FALSE

# x not-element-of S is defined as NOT applied to the result of x is-element-of S
x ∉ S ≣ NOT (x ∈ S)
```

Note there is a logical difference between `x` and a set that contains just `x`
`{x}` (the so called "singleton set")

5 != {5}

CJD says that SQL gets into trouble here with subqueries because sometimes they
represent the element itself and sometimes they represent ta set containing
just that element

up to 8:00

### Aside: math notation shortcuts

elof        ∈
nelof       ∉
opt+shift+o Ø
tribar      ≣

### Aside: ≣ (triple bar)

Triple bar has a number of context dependant meanings in maths
    strictly equal to
    * used in equations where the LHS is being defined not derived
    * used for identical equality of functions e.g. f ≣ g if f(x) = g(x) for all x
    * can mean "if and only if" in logic (⇔ is also used for this)


## Set Theory

* A branch of mathematics
* closely related to logic
* formalizes the concept of a set in terms of certian axioms
    * axiom of extension: S1 and S1 are equal (shorthand S1 = S2) iff they have the same members

Set Theory is built on top of the idea of "equality" and "equality comparators"

It needs the equality tests:

* are two sets equal
* are two element equal?

### Aside axiom
An axiom or postulate is a premise or starting point of reasoning. As
classically conceived, an axiom is a premise _so evident as to be accepted as
true without controversy_.

A venn diagram is representing a set on paper as a circle or rectangle

Defining terms: subset and superset

Note we are defining these, not deriving them

* S2 is a subset of S1 (S2 subsetof S1) iff every element of S2 is also an element of S1
* S1 is a superset of S2 (S1 supersetof S2) iff every element of S2 is an element of S1

* Every superset is a set
* Every subset is a set
    * => every operation we can perform on a set we can perform on subsets and supersets
    * maths takes nothing for granted!

If S1 = S2 then they are supersets and subsets of each other ???

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


Consequences:

Every set is included in itself
Every set is a subset of itself
Every set is a superset of itself
Every set includes itself
Every set includes the empty set

The empty set is a subset of every set
Every set is a superset of the empty set

The universal set is a superset of every set
Every set is a subset of the universal set

We can now define "equality" between S1 and S1 by saying that "S1 and S2 are
equal iff S1 is a subset of S2 and S2 is a subset of S1" i.e. they include each
other.

Don't confuse _set inclusion_ ⊆ with _set membership_ ∈. A set _contains_ its
elements but _includes_ its subsets

## Power sets

The set of all subsets of S is the _powerset of S_ denoted P(S)

{ 2, 3, 5 } // n = 3
{ {}, {2}, {3}, {5}, {2,3}, {2,5}, {3,5}, {2,3,5} } n = 8

If S has cardinality n then P(S) has cardinality 2^n because each element has
two possible states it can have:
1. it is in the set
2. it is not in the set
We have n elements times 2 states => 2^n

Notice that "all the subsets of a set" includes both the empty set and itself

Conclusions
    * the power set represents all the possible combinations of a set of elements
    * P(S) = S for the empty set

UP to end of first video
