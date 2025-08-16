# Memory & Borrowing

Aside:

- rust stack allocates by default
- allocate explicitly onto the heap via `Box<T>`
- rust uses jemalloc as its allocator by default
    - why not use the libc one?

## Pointers

Memory problems come from "aliasing" and "mutation" at the _same time_.

aliasing = two pointers pointing to the same chunk of memory (and having
expectations about what is there) e.g. a pointer to a vector and a pointer to
some element with in it \* makes dependencies non obvious

mutation = changes to objects that cause their memory to be freed e.g. adding
element to vector \* aliasing is fine if everything is static

## 3 Patterns

Rust has 3 patterns of memory management

### 1. Ownership

- The variable that gets assigned when you create an object _owns_ that pointer
  (physical books are good analogy)
- It can give that pointer to something else - then it no longer has it.
    - ? ex of give?
- There is no aliasing because there is only one pointer that is passed around
- When the last owner goes out of scope the object can be destroyed (memory
  freed) automatically by rust
- The idea here is of "giving the book away forever" not "lending the book"
- NB: when you give away a refernce you **cannot** use it anymore! Its
  destruction is no longer linked to yours!

```rust
// this funciton signature says:
// I expect to get ownership of T to use and do with as I please
fn foo(v: T) {
}

fn main() {
    // ...
    foo(thing); // whether an arg is passing ownership or reference, mutable or not is clear from call site too
    // ...
}
```

### 2. Shared borrow

- To borrow is to receive something with the promise of returning it
- This is "lending the book" - the original owner gives a immutable reference to
  1+ others
- This aliasing is ok because there is no mutation possible
- Q: is there an assumtion that the original owner will live longer than the
  things the object is shared with?
- Not giving away ownership, just giving away access to the memory for a little
  while
- While something is borrowed you cannot mutate it either

```rust
// I expect to get a immutable copy of the refernce but I do not take ownership
fn foo(v: &T) {
}

fn main() {
    // ...
    foo(&thing); // whether an arg is passing ownership or reference, mutable or not is clear from call site too
    // ...
}
```

### 3. Mutable borrow

- Not giving away ownership, just giving away access to the memory for a little
  while
- The memory can be changed by the thing I am giving the reference to
- It can only be held by one thing at a time - it is always passing like a token
- eventually it will make its way back to the original owner
- In this case we have mutation but not aliasing - only one thing at a time has
  the "magic token" that allows mutation

```rust
fn foo(v: &mut T) {
}

fn main() {
    // ...
    foo(&mut thing); // whether an arg is passing ownership or reference, mutable or not is clear from call site too
    // ...
}
```
