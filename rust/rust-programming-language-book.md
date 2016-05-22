# The Rust Programming Language

* <https://doc.rust-lang.org/book/>

## Getting started

```rust
fn main() {
    println!("Hello world");
}
```

* the ! suffix on println means that it is a macro not a function call
* the `"hello world"` string is statically allocated
* the ; suffix turns the expression into a statement
* rust is an "expression oriented language"

* Expression
    * An expression is a combination of values, constants, variables, operators, functions that evaluates to a _single value_.
* Statement
    * the smallest standalone element of a programming language that commands the computer to do something
* In early programming languages expressions and statements were clearly different
    * expression = evaulate to a value i.e. pure function, no side effects
    * statement = do something i.e. have a side effect
* Later programming languages blurred this line allowing expressions to have side effects and statements to return a value

In an _Expression oriented language_ nearly every statement is an expression
therefore returns a value - these "statements that return a value" are
"expression statements". Because they return a value they can be nested inside
other expression statements

There are only 2 kinds of statements in rust, everything else is an expression:

1. Declaration statements (let, anything that is an item declaration, there may be others too)
2. Expression statement (use `;` to throw away return value)

* most things in rust are expressions in the more modern sense i.e. they return a value but can have side-effects
* `;` turns any rust expression into an "expression statement" by throwing away its return value and returning `()` (the unit type) instead.
* Rust's grammar expects "statements to follow statements" so we use `;` to turn expressions into statements to keep it happy

Declaration statements do **not** return anything so you cannot do

```rust
let x = y = 34i;
```

```
let mut x = 23i; // a statement
x = 45;          // an expression that returns `()` the
                 // unit type (so return value not very useful)
```



## Chapter 4: Syntax and semantics

### 4.7 Ownership


```rust
fn foo() {
    let v = vec![1, 2, 3];
}
```

* allocates {vector-object} on the stack and puts the actual vector data on the heap
* vector size can change at runtime
* a vector is
    1. stack memory
        * pointer to heap memory
        * length of vector
    2. heap memory
        the actual data in the vector
* the binding `v` "owns" that vector (both the bit on the stack and the heap)
* we can think of the stack memory + heap memory for the vector as a "resource"
    * => a resource can be multiple chunks of memory
* rust requires that there be _exactly_ one binding to a resource at any given time
* when `v` goes out of scope rust will clean up the memory owned by it on both stack and heap

```rust
fn foo() {
    let v = vec![1, 2, 3];

    let mut v2 = v; // this moves ownership of the resource from binding v to binding v2
    // conceptually this does a bitwise copy of the stack part of v but does not change the heap part
    // the optimizer might remove the copy eventually

    let x = vec![1, 2, 3];
    stuff(x); // this moves ownership of x to the vv binding in stuff and we don't get it back!

    // any attempt to use x here will be an error because we moved ownership to
    // stuff and there is no "automatically getting back ownership" for moves
}


fn stuff(vv: Vec<i32>) {
}
```
* this function takes ownership of its argument
* it does not return a value so the resource owned by `vv` will be deleted at the end of this function


The Copy trait

* some data is simple enough that rust doesn't have to enforce the ownership rules e.g. integers
* primitive data types implement the `Copy` trait which means that you can use them after a move
