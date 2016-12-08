# Arrays, Vectors, Slices

## Arrays

* are fixed size (immutable) list of elements of the same type
* have type `[T; N]` where T is element type and N is size

```rust
let x = [1,2,3,4,5];
let x: [u8; 5] = [1,2,3,4,5]; // with full type signature
```

* as with other types, you can create a statically allocated array by using `static` keyword instead of `let`
    * reminder: lifetimes are for references only, not for direct bindings to data

## Vectors

> Vec is and always will be a (pointer, capacity, length) triplet. No more, no
> less. The order of these fields is completely unspecified, and you should use
> the appropriate methods to modify these. The pointer will never be null, so
> this type is null-pointer-optimized.

* A contiguous growable array type, written Vec<T> but pronounced 'vector.'
    * note that rust will garantee that the array for a vector is *contigious*
* full name is `collections::vec::Vec`
* vectors are a "resource" in rust i.e. a thing which may use/own memory in different places e.g. stack, heap

```rust
let v = vec![1, 2, 3];
let mut v1 = Vec::new(); // note no type info provided on this line
v1.push(1)


let mut v1 = Vec::new(); // note no type info provided on this line
v1.push(1);
v1.push(2);
v1.push(3);
// or
// let v1 = vec![1, 2, 3];

// let v2 = vec![0, 0, 0, 0];
// or
// let v2 = vec![0;4];
// or
// let v2 = Vec::new();
// v2.push(0);
// v2.push(0);
// v2.push(0);
// v2.push(0);

// v1[1] // this works because Vec implements the `Index` trait
// // access by index will panic! if it is out of bounds
// v1.pop() // returns an Option to indicate it can fail

let v3 = Vec::with_capacity(100);
```

* allocates a {vector-object} on the stack and puts the actual vector data on the heap
* a vector is
    1. stack memory
        * pointer to heap memory
        * length of vector
        * a capacity
    2. heap memory
        * a buffer of the actual data in the vector

> If a vector's length exceeds its capacity, its capacity will automatically be increased, but its elements will have to be reallocated.

> If a vector's length exceeds its capacity, its capacity will automatically be increased, but its elements will have to be reallocated.

* mutating your vector *could* cause it's heap memory to be completely reallocated!!!
    * if this is a problem you can guard against this by creating the vector with `Vec::with_capacity(usize)`
* vectors can be mutable - buffer size can change at runtime

* the binding `v` "owns" that vector (both the bit on the stack and the heap)
* we can think of the stack memory + heap memory for the vector as a "resource"
    * => a resource can be multiple chunks of memory
* rust requires that there be _exactly_ one binding to a resource at any given time
* when `v` goes out of scope rust will clean up *all* the memory owned by the resource on both stack and heap

* a rust program in RAM is a bunch of resources, each of which is owned by exactly one binding at any time
* when bindings are mentioned anywhere except the LHS of assigment they
* when you have a local variable `x` and you call a function with sig `fn
  do_thing(y: String)` as `do_thing(x)` then the resource is moved from your
  locl `x` binding to the `y` binding within the function - you will never get
  it back! (unless you return a value from `do_thing()` and manually assign it
  to `x`

```rust
fn stuff(vv: Vec<i32>) {
    // this function has a signature where it takes ownership of its argument
    // it does not return a value so the *resource* owned by `vv` will be deleted at the end of this function
} // vv goes out of scope here

fn main() {
    let v = vec![1, 2, 3];

    let mut v2 = v;
    // this moves ownership of the resource from binding v to binding v2
    // conceptually this does a bitwise copy of the stack part of v but does not change the heap part
    // (the optimizer might remove the copy)

    let x = vec![1, 2, 3];
    stuff(x); // this moves ownership of x to the vv binding in stuff and we don't get it back!

    // any attempt to use x here will be an error because we moved ownership to
    // stuff and there is no "automatically getting back ownership" for moves.
}
```

## Slices

* Unlike vectors, slices are read-only
* slices are created via `&[T]` e.g. `&[i32]` is a slice of ints, `&[String]` is a slice of String objects

```
let v = vec!(0, 1);
let x : &[usize] = &v; // create a slice which refers to same resource as v

let some_slice
```

> In Rust, it's more common to pass slices as arguments rather than vectors
> when you just want to provide a read access. The same goes for String and
> &str.
