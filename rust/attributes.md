# Attributes

Have 3 possible forms

1. `#[attribute_name]`
    * the name of the attribute surrounded by `#[]`
    * e.g. `#[test]`
2. `#[identifier=literal]`
    * identifier followed by = followed by a literal
    * e.g. `#[cfg=test]`
3. `#[identifier(attr3,identifier2=literal2)]`
    * an identifier followed by a paren wrapped list of other attributes (of either type 1. or 2. above)

Are applied to "items". An item is one of 8 things:

1. functions `fn`
2. modules `mod`
3. type definitions `struct`
4. structures `struct`
5. enumerations `enum`
6. static items
7. traits
8. implementations `impl`

These are created with this (TODO: partial) list of keywords:

    fn
    mod
    struct
    enum
    impl

Item declarations are *not* expressions
* A rust source file is simply a collection of 0 or more items (optionally prefixed with attributes)

```rust

#[attribute_name]
#[identifier=literal]
#[identifier(attr1,identifier2=literal2)]

// apply attribute to the "item" that encloses it in the source
#![blah]

// apply attribute to the "item" that follows it in the source
#[blah]
```
