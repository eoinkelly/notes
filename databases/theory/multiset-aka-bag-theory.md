# Multiset or bag

https://en.wikipedia.org/wiki/Multiset

- The mathematical model of SQL tables
- multiset also called _bag_ or _mset_
    - Knuth proposed some other names: _list, bunch, bag, heap, sample, weighted
      set, collection, and suit_
- is a variant of a _set_ but theorems that apply to sets do not necessarily
  apply to multisets
- allows multiple instances of its elements
- _multiplicity_ is a positive integer representing the number of instances of a
  particular element in a multiset
- multisets do not have an ordering
- cardinality of a multiset is found by summing the multiplicities (i.e.
  counting the elements, not counting the unique elements)

Consider:

$$\{a, b\}$$ $$\{b, a\}$$

- these can be viewed as a _set_
- these can be viewed as a _multiset where each element has multiplicity = 1_
- they are equal to each other as both a set and a multiset

Consider

$$\{a, b\}$$ $$\{a, a, b\}$$

If these were sets they would represent the same set but viewed through the
multiset lens they are different.
