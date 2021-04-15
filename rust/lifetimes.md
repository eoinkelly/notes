# Lifetimes

* lifetimes only apply to _references_ (i.e. borrowed variables)
* lifetime syntax is about connecting the lifetimes of function parameters to function return values
* rust will usually be able to figure out lifetimes on its own but it sometimes needs help
* lifetime _annotations_ (the `'a` etc.)
    * do not change how long references live - they just give the rust compiler some hints
    * describe the **relationship** of multiple lifetimes to each other - practically you'll never see code with just one variable with a lifetime annotation (I think!)
    * are a kind of **generic** parameter to the function
        * they need to be introduced within `<>` before they can be used in the function signature
        * they do not refer to the actual lifetime of any variable!
        * rather they are a constraint that the actual lifetimes must satisfy
        * the compiler knows the actual lifetimes and we have give it some constraints with our lifetime annotations so it will compile if it can fit the actual lifetimes into our constraints
        * e.g. if you pass multiple references to a function

* When returning a reference from a function, the lifetime of the return value needs to match the lifetime for one of the parameters
    * if you return an arbitrary reference created within the function then it will be dangling immediately after the function returns

what can functions in rust return

they can move their return value back to the caller
if they return a reference, it must be a reference to one if their params
it makes no sense to return a reference to something created in the function


```rust
// the concrete lifetime that rustc assigns to 'a will be the smaller of the lifetimes of x and y
// then it can use that as the lifetime of the return value
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() {
        x
    } else {
        y
    }
}
```






> Ultimately, lifetime syntax is about connecting the lifetimes of various
> arguments and return values of functions. Once they're connected, Rust has
> enough information to allow memory-safe operations and disallow operations
> that would create dangling pointers or otherwise violate memory safety.

* Every **reference** in rust has a lifetime. Only references have lifetimes
* most lifetimes can be inferred by compiler so we don't have to type them
* lifetimes are similar to generic types
    * generic type annotations let us provide enough type to the compiler
      without specifying a concrete type so the compiler can ensure that type
      safetly will be maintained at runtime
    * lifetime annotations let us provide enough lifetime into to the compiler
      so it can ensure that all references used will definitely be valid

When we annotate lifetimes with `'a` etc. they are **generic** lifetimes i.e.
they do not refer to a particular lifetime, rather they are a constraint which
many concrete lifetimes can satisfy.

* Lifetime constraints are not interpreted the same way as generic type constraints
    * if a generic type `T` is applied to multiple values in a function
        signature then `T` gets mapped to a single concrete type
    * if a generic lifetime `'a` is applied to multipel values in a
        function signature then it gets mapped to whichever of the lifetimes
        is shortest

These behave differently because types don't always nest but scopes do so if
two values have the same lifetime annotation but have different concrete
lifetimes then rust can resolve the smaller of those concrete lifetimes and use
that for the annotation

Rule
If you return a reference from a rust function then it must be one of the arguments

* Rule: the lifetime parameter of a return type **must** match the lifetime parameter of one of the arguments
    * If you return a reference which doesn't refer to one of the arguments
      then it must be a reference to something created within the function
      which will go out of scope at the end of the function so that reference
      will be dangling.
* If you have a function which creates a value, it should pass the value and
  ownership back to the caller. When the caller owns the value it then becomes
  responsible for cleaning it up.


// When concrete references are passed to longest, the concrete lifetime that gets substituted for
// 'a is the part of the scope of x that overlaps with the scope of y. Since scopes always nest,
// another way to say this is that the generic lifetime 'a will get the concrete lifetime equal to
// the smaller of the lifetimes of x and y. Because we've annotated the returned reference with the
// same lifetime parameter 'a, the returned reference will therefore be guaranteed to be valid as
// long as the shorter of the lifetimes of x and y.
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() { x } else { y }
}

## Lifetime elision rules

Rust will provide lifetime parameters for references whenever it can but sometimes it cannot

Terminology

* input lifetimes = lifetime parameters on function and method arguments
* output lifetemes = lifetime parameters on return values

1. Each parameter that is a reference gets its own lifetime parameter.
    * In other words, a function with one parameter gets one lifetime
      parameter: `fn foo<'a>(x: &'a i32)`, a function with two arguments gets two
      separate lifetime parameters: `fn foo<'a, 'b>(x: &'a i32, y: &'b i32)`, and
      so on.
1. If there is exactly one input lifetime parameter, that lifetime is assigned
   to all output lifetime parameters
    * `fn foo<'a>(x: &'a i32) -> &'a i32`
1. If there are multiple input lifetime parameters, but one of them is &self or
   &mut self because this is a method, then the lifetime of self is assigned to
   all output lifetime parameters.
    * This makes writing methods much nicer.

If the compiler can't fill in all lifetimes in a function signature after
following these rules it will throw an error and ask you to do it.

## 'static lifetime

There is one special lifetime we need to discuss: 'static. The 'static lifetime
is the entire duration of the program. All string literals have the 'static
lifetime,
