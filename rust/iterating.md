# Iterating

https://doc.rust-lang.org/std/iter/index.html

* The `iter` module is part of the standard library
* available in the following crates:
    1. core (the crate which is the dependency free foundation of the standard lib)
    2. std (the crate that implements the rust standard library)
* Unless you have a reason to need only `core` then you should use the `std` namespace to import it.

You must always `use`

* In rust you have to `use` every module to access it but rust has the concept of "The prelude" which are a set of things which are automatically "used" for your rust files
* Each new edition (e.g. v1, 2015, 2018, 2021) of rust (usually) expands the prelude a bit


`iter` is made up of

1. Structs
2. Traits
3. Functions
    * these are kinda "class methods"
    * functions on the module itself, not part of a trait,

THe `Iterator` trait is the most important
https://doc.rust-lang.org/std/iter/trait.Iterator.html

There is a naming convention for creating iterators from a collection

* `iter()` iterates over `&T`
* `iter_mut()` iterates over `&mut T`
* `into_iter()` iterates over `T` (i.e. it moves ownership)

These methods are implemented by various collections and things in the standard library

Aside: rust docs show the methods in the order defined in the source

    TODO: diff between copy and clone

Aside copy vs clone

* copy trait
    * https://doc.rust-lang.org/std/marker/trait.Copy.html
    * a type has the Copy trait if it's values can be duplicated by copying bits
    * if a type implements `Copy` then variable bindign has copy semantics not move semantics
    * happen implicitly as part of assignment `y = x`
    * is always a bitwise copy - you cannot overload it
    * you can expect it to be "inexpensive"
    *
* clone
    * https://doc.rust-lang.org/std/clone/trait.Clone.html
    * explicit action i.e. you have to call `.clone()`
    * may or may not be "expensive"
    * you can reimplement it and run arbitrary code
    * any type which is Copy is also Clone

### std::iter::Iterator methods required methods:

You must implement these to implement the `std::iter::Iterator` trait

* `next()` - advances the iterator and returns the next value

### std::iter::Iterator methods provided methods:

you don't have to implement these to implement Iterator but you can choose to if you want to override the default (sometimes not very useful) behaviour.

* `count()` - consume the iterator until you get to `None`, return count of iterations to do so
* `enumerate` - create a new iterator which will yields `(i, val)` where `val` is the value from the original iterator and `i` is an index starting at 0
    * this is rust's `each_with_index`
* `copied` - create iterator which copies all it's elements - turn an iterator over `&T` to one over `T`
* `cloned` - create iterator which clones all it's elements - turn an iterator over `&T` to one over `T`
* `collect()` turn an iterator into a collection
    * very general so sometimes you need the _turbofish_ syntax `::<>` to hint to type inferenve what you mean
    * uses
      * to transform a collection into another collection you call `iter` on it, do your transformations and then call `collect` to go back into a collection at the end
      * you can use creatively e.g. a collection of `char` can collect into a `string`
      ```
      .collect(); // collect into some type
      .collect::<Vec<usize>>(); // be explicit about what we are collecting into
      .collect::<Vec<_>>(); // partial type hint - works becuse collec() only cares about the container
      ```
* `partition` splits collection into two collections
    * you don't need to also call `collect()`
* `eq(other)` are the elements in this iterator equal to other
    * presumably it consumes both?
* `eq_by(other)` check for equality with provided closure
* `inspect(closure)` gives the value to your closure and passes the value to the next in chain
    * lets you put some `println` into an iterator chain
* `by_ref()` borrow an iterator rather than consuming it
    * useful when you want to do something with the iterator but still keep ownership in the original variable
* `all()` - check if all elemetns make the closure return true
* `any()` - check if any element make the closure return true