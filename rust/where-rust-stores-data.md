# Data storage in a rust process

Possible places you can store values in a process

1. stack
2. heap
3. hard-coded into instruction stream
4. in the DATA or BSS area of the executable

Rust thinks in terms of "resources"

- each resource can have stack & heap memory
- each resource can have only 1 binding at any time

The areas in more detail:

1. stack
    - rust stack allocates when you use `let` to introduce a binding
        - it is possible that rustc will use registers rather than stack if it
          can - does it?
1. heap
    - even when you allocate on the heap, you need a pointer on the stack to
      find it in the heap (so there is no such thing as a "just heap
      allocation")
    - `Box::new(T)` lets you explicitly allocate the type T on the heap
        - there will also be a value on the stack pointing to it
1. hard-coded into instruction stream
    - `const` does this for rust
        - you must _always_ supply a type annotation for `const`
    - it inlines the value directly into the instruction stream
    - consequences:
        - references to the same `const` **may** not point to the same place in
          memory (depends on what the compiler decides to do given the size of
          storage required for the value)
        - the `const` value doesn't have a necessairly have a single memory
          address
        - it makes no sense for `const` values to be mutable
1. in the DATA or BSS areas of the executable
    - `static` does this
    - provides a sort of "global variable" functionality
    - you must _always_ supply a type annotation for `static`
    - you can make the value mutable (by specifying `mut` for the binding as
      usual) but you can only mutate within an `unsafe` block because it is
      effectively thread global data and access from mutiple threads could cause
      problems.

Tip: you almost always want `const` instead of `static` - it is more efficient
because it opens possibility of rustc re-using constant values across crates.
rustc is (presumably) smart enough to store the constant in one place in the
instruction steam if that can be done.

```rust
const TITLE: &'static str = "Hello there";  // explicit 'static lifetime is required
const TITLE: &str = "Hello there";          // does not compile
const TITLE = "Hello there";                // does not compile

static TITLE_2: &'static str = "Hello there"; // explicit 'static lifetime is required
static TITLE_2: &str = "Hello there";         // does not compile
static TITLE_2 = "Hello there";               // does not compile
```
