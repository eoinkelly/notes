


```rust
println!("hi there");
// "hi there" is statically allocated
```

Statically allocated: ???




# How do I implement a "class" in rust?

Lets try implementing this simple ruby class in Rust:

```ruby
class Foo
    def initialize(a, b)
        @a = a
        @b = b
    end

    def addtastic
        a + b
    end
end

f = Foo.new(4,6)
puts "Result is: #{f.addtastic}"
```

## Background

Rust provides 4 componound data types

1. tuple
    * an ordered list of a fixed size
    * the structure has no name
    * the elements have no names
    * elements are retrieved by destructuring
2. struct
    * the structure has a name
    * each element has a nane
    * elements are retrieved based on name
    * `struct` with `impl` mimics what we have in ruby as class syntax
3. tuple-struct
    * the structure has a name
    * the elements have no names
    * it is almost always better to use a real struct instead of a tuple-struct
    * the main use case for tuple-structs is to alias one type with another
4. enum
    * a "sum type" i.e. type X is exactly one of the values from the provided set
    * can be simple set of values
    * each value can take parameters
    * `match` expression can destructure the values contained in an enum value
    * can be used to implement option type

`=` in rust reads as _take what is on the RHS and bind to the LHS breaking apart the structure of the RHS as required to make it happen_

```rust
// create a tuple
let x = (2i, 4i, 6i);

// create tuple-struct (named tuple)
struct Color(int, int, int);
struct Point(int, int, int);

let black = Color(0,0,0);
let origin = Point(0,0,0);

// Inches is a tuple that contains one element
struct Inches(int);

let length = Inches(10);

let Inches(inner_int) = length; // unbox the tuple with a destructuring let
```

## Solution

```rust
struct Foo {
    a: int,
    b: int
}

impl Foo {
    fn addtastic(&self) -> int {
        self.a + self.b;
    }
}

let f = Foo { a: 4, b: 6 }
println!("Result is: {}", f.addtastic());
```

### 3 selves

A function within an `impl` block is an instance method if it takes one of

1. `self`
2. `&self`
    * most common
3. `mut &self`

as its first parameter. When you call `thing.do_something(a,b);` it actually
passes a reference to `thing` as the first parameter to `do_something`

QUESTION: what is the difference between them?
    it seems like the `.` syntax is just sugar for calling functions and the normal rules of references and borrowing apply (i don't yet understand those rules so cannot be sure)

## Extras

* Can I have multiple `impl` blocks for a single `struct`?
    * Yes.
* Can I override methods defined in a previous `impl` block?
    * No - compiler error.


# How can I implement ruby block syntax in rust?

```ruby
def simple_wrap(a, b, &block)
    puts "Header: #{a}"
    yield(3)
    puts "Footer: #{b}"
end

# interestingly the method defn above is not enough info to implement this in
# rust - I also need to see an implementation

simple_wrap(4, 5) do |x|
    puts "Body: #{x}"
end

```


```rust
fn simple_wrap(a: int, b: int, f: |int| ) {
    println!("Header: {}", a);
    f(3);
    println!("Footer: {}", b);
}

simple_wrap(4i, 5i, |x| { println!("Body: {}", x); }
```

TODO: finish this

# Crates & Modules

* Crates are a _compilation unit_ in rust. They are what is
    * compiled
    * linked against
    * loaded at runtime
    * versioned
    * distributed

* Rust compiles one crate at a time
* The rust compiler takes as input a single crate in source form and outputs a single create in binary form
* Each crate produces either a library or an executable
* crates can depend on other crates
* the rust compiler is always invoked with a _single_ `.rs` file as input.
    * the processing of that file may result in loading other source files as modules
* a rust source file
    * is just _items_ and _attributes_
    * defines a single module, the name of which is set from _outside_ the file.
    * the name can be set by
        1. the crate the file is in
        2. an explicit `mod_item` (???) in a referencing source fil
* each crate contains a heirarchy of modules

    crate
    |-> crate_root module
        |-> module
        |-> module
            |-> module
            |-> module
                |-> module
                |-> module
        |-> module

* the top level module ("crate_root") is anonymous from the pov of paths within the module
* rust does not _enforce_ a relationship between source files and modules
* a module without a body is loaded from an external file
    * defaults to the name of module with `.rs` extension
* crates that contain an executable contain a `main` function
* a crate taht contains a `main` function _can_ be compiled to be an executable
    * the main function must take no args and return unit `()`
* all functions in module are private unless "exported" with the `pub` prefix

```rust
// Examples of loading other modules


// loads "my_bleh.rs"
mod my_bleh;

mod alpha {
    // loads "alpha/beta.rs"
    mod beta;

    // loads "other/oddity.rs" because attribute overrides default
    #[path = "other/oddity.rs"]
    mod oddity;
}
```

### View items

* 2 types of "view items"
    1. `extern crate foo;`
    2. `use bar;`
* they don't define new items - they just change other items visiblitiy

#### `extern crate`

* binds an extenal crate to the given identifier
* remember that an external crate is a separate compiled unit - a library file
    * Q: are they statically/dynamically linked by default?
```rust
extern crate foo as foo;
extern crate foo; // shorthand for line above

extern crate foo as bar; // can change name of binding
```

#### `use`

* shortens paths so you have less typing to do
* NB: does NOT declare linkage depenendency with other crates!!! (extern crate does this)

TODO continue at http://doc.rust-lang.org/0.12.0/reference.html#use-declarations

```rust

// loads "my_bleh.rs"
mod my_bleh;

mod alpha {
    // loads "alpha/beta.rs"
    mod beta;

    // loads "other/oddity.rs" because attribute overrides default
    #[path = "other/oddity.rs"]
    mod oddity;
}

fn main() {
    blah::do_thing();
}

mod blah {
    pub fn do_thing() {
        println!("a thing");
    }

    // not exported so private
    fn private_thing() {
        println!("a private thing");
    }
}

mod blah {
    pub fn do_another_thing() {
        println!("a thing");
    }

    // modules can nest arbitrarily
    mod sub_blah {
        // ...
    }
}
```

* You can re-open a module multiple times. This is very handy if you change the
  "preprocessor directive thingies" that go with it e.g. you can tell rust not
  to compile some bits for certain platforms
* You import a crate with `extern crate some_crate;`
    Q: what exactly does this do? is it like C extern?

    TODO: make a crate that spans multiple files and has multiple modules

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

# How do I do ruby class inheritance in rust

```ruby
class Bar
    def some_fun
    end
end

class Foo < Bar
    def thing_from_foo
    end
end
```

    TODO: do this

# How do I do ruby module inclusion in Rust?

```ruby
module Bar
    def some_fun
    end
end

class Foo
    include Bar
    def thing_from_foo
    end
end
```

    TODO: do this

# How do I do ruby module extension in Rust?

```ruby
module Bar
    def some_fun
    end
end

class Foo
    extend Bar
    def thing_from_foo
    end
end
```

    TODO: do this

# Memory

## Pointers

Memory problems come from "aliasing" and "mutation" at the _same time_.

aliasing = two pointers pointing to the same chunk of memory (and having expectations about what is there)
    e.g. a pointer to a vector and a pointer to some element with in it
    * makes dependencies non obvious

mutation = changes to objects that cause their memory to be freed e.g. adding element to vector
    * aliasing is fine if everything is static

## 3 Patterns
Rust has 3 patterns of memory management

### 1. Ownership

* The "thing" that creates an object _owns_ that pointer (physical books are good analogy)
* It can give that pointer to something else - then it no longer has it.
    * ? ex of give?
* There is no aliasing because there is only one pointer that is passed around
* When the last owner goes out of scope the object can be destroyed (memory freed) automatically by rust
* The idea here is of "giving the book away forever" not "lending the book"
* NB: when you give away a refernce you **cannot** use it anymore! Its destruction is no longer linked to yours!

```rust
// this funciton signature says:
// I expect to get ownership of T to use and do with as I please
fn foo(v: T) {
}
```

### 2. Shared borrow

* To borrow is to receive something with the promise of returning it
* This is "lending the book" - the original owner gives a immutable reference to 1+ others
* This aliasing is ok because there is no mutation possible
* Q: is there an assumtion that the original owner will live longer than the things the object is shared with?
* Not giving away ownership, just giving away access to the memory for a little while
* While something is borrowed you cannot mutate it either

```rust
// I expect to get a immutable copy of the refernce but I do not take ownership
fn foo(v: &T) {
}
```

### 3. Mutable borrow

* Not giving away ownership, just giving away access to the memory for a little while
* The memory can be changed by the thing I am giving the reference to
* It can only be held by one person at a time - it is always passing like a token
* eventually it will make its way back to the original owner
* In this case we have mutation but not aliasing - only one thing at a time has the "magic token" that allows mutation

```rust
fn foo(v: &mut T) {
}
```


