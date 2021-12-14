# Strings in rust

http://www.steveklabnik.com/rust-issue-17340/
https://play.rust-lang.org/

> A String is made up of three components: a pointer to some bytes, a
> length, and a capacity. The pointer points to an internal buffer
> String uses to store its data. The length is the number of bytes
> currently stored in the buffer, and the capacity is the size of the
> buffer in bytes. As such, the length will always be less than or
> equal to the capacity.
>
> This buffer is always stored on the heap

* A rust string is
    * a sequence
    * of unicode scalar values
    * encoded as
    * a stream of UTF-8 bytes
* All strings are garuateed to be validly encoded UTF-8 sequences
* Rust strings are not null terminated so can contain null bytes

There are two types of rust string

1. `&str` (pronounced "string slice")
1. `String`

## String slice aka &str

* string slices are
    * fixed size
    * immutable (slices can be mutable or immutable but string slices are immutable (`&str` not `&mut str`)
* string literals are of type `&str` (a reference to an `str`)
* string literals have a lifetime of  `&'static`
* a string slice (`str`) is a struct containing
    1. a pointer to the beginning of some data
    2. a length
* => string slices are a "view" into an already allocated string

```rust
let foo = "blah"; // :: &'static str

```

You can turn a stack allocated array of bytes into an `&str`

## Heap allocated string aka String

* a heap allocated string
* is growable
* is always UTF-8
* can be coerced to/from `&str` (via `to_string()` and `as_slice()`)
    * note that converting to `&str` is cheap but converting to `String`
      requires allocation so is expensive

```
let mut foo = "blah".to_string(); // turn the &str into a String

println!("{}", foo);
foo.push_str("more blah")
println!("{}", foo);

foo.as_slice(); // coerce String into &str
```

# Indexing over strings

* rust strings are not array of bytes like C strings are - they are UTF-8 encoded sequences
    * => not all characters are one byte

> Indexing is intended to be a constant-time operation, but UTF-8 encoding does
> not allow us to do this. Furthermore, it's not clear what sort of thing the
> index should return: a byte, a codepoint, or a grapheme cluster. The
> as_bytes() and chars() methods return iterators over the first two,
> respectively.

There are three levels of unicode (and hence three levels of encodings

1. code units (the underlying data type used to store everything)
2. code points (unicode scalar values)
3. graphemes (visible characters)


There are three iterators to allow access at different unicode levels:

1. UnicodeSegmentation::graphemes(s, true)
    * yields each visible character from the string as type `&str`
      (each visible character may be more than one `char` so it must
      be a string)
    * requires the unicode_segmentation crate
        * https://unicode-rs.github.io/unicode-segmentation/unicode_segmentation/index.html
    * this is usually what you want when you want to "get each character from a string"
1. chars()
    * yields each unicode code point in the string as type `char`
1. .bytes()
    * An iterator over the bytes of a string slice.
    * yields each byte in the string as type `u8`
1. `as_bytes()`
    * returns a byte slice of the string's content
    * yields each byte in the string as type `&u8`

TODO: is bytes() or as_bytes() prefered - the rust book uses as_bytes()


# Making functions which can take either type of string

You have two options for making a function which is generic over both string types

1. Take `&str` and convert any `String` instances to `&str` at the call site with `as_slice()`
2. Use the `Str` trait
    * -- more complicated

# Compare differnt string types

Prefer converting both to `&str` because it is cheaper than converting both to
`String` (conversion to `String` allocates in heap)

```rust
#[macro_use] extern crate t_bang;
use t_bang::*;

extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;

fn main() {
    println!("A silly rust program to dump types");

    let x = 33.4;
    pt!(x);

    let s = "foo";
    pt!(s);
    pt!(s.to_string());

    let heap_string = "something".to_string();
    pt!(heap_string);

    let s = "u͔n͈̰̎i̙̮͚̦c͚̉o̼̩̰͗d͔̆̓ͥé";

    // iterate across as bytes
    for x in s.bytes() {
        println!("{} has type {}", x, t!(x));
    }

    // iterate across as unicode codepoints
    for x in s.chars() {
        println!("{} has type {}", x, t!(x));
    }

    // iterate across as unicode grapheme clusters
    for x in UnicodeSegmentation::graphemes(s, true) {
        println!("{} has type {}", x, t!(x));
    }
}

```

Rust  stdlib does not include the ability to iterate across a string by grapheme cluster (aka "human letters") - it was in `std` but was removed. Full unicode support is now in these crates:

* https://crates.io/crates/unicode-normalization
* https://crates.io/crates/unicode-segmentation (allows iteration)
* https://crates.io/crates/unicode-width




