# Debugging

## Print debugging

### dbg! macro

`dbg!(a = 213)` prints the given expression and it's result to stderr and also
returns the result so you can insert it in other expressions

It does take ownership of the expression so pass in references unless the types
involved implement `Copy`

### format! macro based strings

Everything based on `format!` - docs: https://doc.rust-lang.org/std/fmt/

- `print!` formats to stdio
- `println!` formats to stdio with newline appended
- you can do the usual padding stuff you would expect from a format

```rust
println!("{}", x); // uses Display trait
println!("{:?}", x); // uses Debug trait
```

argument syntax: https://doc.rust-lang.org/std/fmt/#syntax

```
{} // requires std::fmt::Display trait be implemented for that type
{:?} // requires std::fmt::Debug trait be implemented for that type
{:#?} // pretty print std::fmt::Debug
```

println! uses format! internally

When requesting that an argument be formatted with a particular type, you are
actually requesting that an argument ascribes to a particular trait.

This allows multiple actual types to be formatted via {:x} (like i8 as well as
isize). The current mapping of types to traits is:

- nothing ⇒ Display
- ? ⇒ Debug
- o ⇒ Octal
- x ⇒ LowerHex
- X ⇒ UpperHex
- p ⇒ Pointer
- b ⇒ Binary
- e ⇒ LowerExp
- E ⇒ UpperExp

=> `format!` isn't just for printing as strings or debugging - it is a general
purpose set of shortcuts for format conversion
