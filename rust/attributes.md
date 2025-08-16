# Attributes

Source: <https://doc.rust-lang.org/beta/reference.html#attributes>

- syntax inspired by C#

> An attribute is a general, free-form metadatum that is interpreted according
> to name, convention, and language and compiler version.

Attributes are applied to "items". An item is one of 8 things:

1. functions `fn`
2. modules `mod`
3. type definitions `struct`
4. structures `struct`
5. enumerations `enum`
6. static items
7. traits
8. implementations `impl`

Note: Item declarations are _not_ expressions

- => they do not return a value.
- => You do not need a `;` after them

A rust source file is simply a collection of 0 or more items (optionally
prefixed or containing attributes)

Attributes have two forms

1. `#[attribute_name]`
    - apply the attribute to the _item_ immediately after it
1. `#![attribute_name]`
    - apply the attribute to the enclosing _item_

Attributes arguments have 3 possible forms

1. `#[attribute_name]`
    - the name of the attribute surrounded by `#[]`
    - e.g. `#[test]`
2. `#[identifier=literal]`
    - identifier followed by = followed by a literal
    - e.g. `#[cfg=test]`
3. `#[identifier(attr3,identifier2=literal2)]`
    - an identifier followed by a paren wrapped list of other attributes (of
      either type 1. or 2. above)

```rust

#[attribute_name]
#[identifier=literal]
#[identifier(attr1,identifier2=literal2)]

// apply attribute to the "item" that encloses it in the source
#![blah]

// apply attribute to the "item" that follows it in the source
#[blah]
```

# #[inline]

https://doc.rust-lang.org/beta/reference.html#inline-attributes

> The inline attribute suggests that the compiler should place a copy of the
> function or static in the caller, rather than generating code to call the
> function or access the static where it is defined.

> The compiler automatically inlines functions based on internal heuristics.
> Incorrectly inlining functions can actually make the program slower, so it
> should be used with care.

```
#[inline] // *ask* the compiler to inline
#[inline(always)] // *ask* the compiler to always inline
#[inline(never)] // *ask* compiler to never inline
```

- makes compiles slower
- you might be better off using rust "link time optimization" instead

From a stackoverflow answer:

> If you really want the fastest binary the compiler can make, you can/should
> use link-time optimisation (rustc -C lto) which has access to all of the
> crate's Rust dependencies, in inlinable form (including things without
> `#[inline]` attributes). This is very slow to compile, but it's isomorphic to
> adding #[inline] to everything and I'd guess that the attributes on everything
> is even slower than -C lto.

> `#[inline]` should be preferred to be used only on performance-critical
> things; e.g. putting #[inline] on most functions doing IO will be absolutely
> pointless for run-time performance (and just drag compile-time performance
> down the drain).

## #[cfg(STUFF)]

https://doc.rust-lang.org/beta/reference.html#conditional-compilation

- `cfg` is used for conditional compilation

```rust
// only compile when compiling the test harness (`rustc --test`)
#[cfg(test)]
```
