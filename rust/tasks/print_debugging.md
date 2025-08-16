# Task: basic print debugging

- The various printing options use `format!` internally
- Argument syntax: https://doc.rust-lang.org/std/fmt/#syntax
- how format args (within `{}`) map to traits
    - nothing ⇒ Display
    - ? ⇒ Debug
    - o ⇒ Octal
    - x ⇒ LowerHex
    - X ⇒ UpperHex
    - p ⇒ Pointer
    - b ⇒ Binary
    - e ⇒ LowerExp
    - E ⇒ UpperExp
- See the log crate for logging with the usual set of features

```rust
// dbg! is for "quick and dirty" debugging
let ret_val = dbg!(<a-rust-expression>)
// prints the value of the expression and also returns it
// requires the value to implement std::fmt::Debug
// dbg! does take ownership of the expression so pass in references unless the types involved implement `Copy`

asset_eq!(a, b);

// print value without newline
print("{}", x);

// print a value (with a newline)
println!("{}", x); // requires x implement std::fmt::Display trait

// print a debugging dump of a value
println!("{:?}", x); // requires x implement std::fmt::Debug trait
println!("{:#?}", x); // requires x implement std::fmt::Debug trait
```
