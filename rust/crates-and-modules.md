# Crates & Modules

- crates contain modules
- the name of the craete is the name of the "root module" in the crate
    - => every crate contains at least one module of the same name
- when we define modules inline in a single file we declare them with
  `mod NAME {}`
- there is ALWAYS a module declaration
- when we place module code in other files we STILL NEED TO DECLARE THEM with
  `mod NAME;`
    - rust uses a name convention to map from that module declaration to the
      file on disk
        - `lib/NAME.rs`
        - `lib/NAME/mod.rs`
    - we don't need to re-declare the module in the other files
- the module that contains the submodule needs to declare it
- declaraing a module in a module also import the name of the module as a symbol
  to use

```
mod foo;

// does an implicit 'use foo;' e.g.

mod foo;
use foo;

```

There are two ways to make multiple module crates

- Crates are a _compilation unit_ in rust. They are what is
    - compiled
    - linked against
    - loaded at runtime
    - versioned
    - distributed

- Rust compiles one crate at a time
- The rust compiler takes as input a single crate in source form and outputs a
  single create in binary form
- Each crate produces either a library or an executable
- crates can depend on other crates
- the rust compiler is always invoked with a _single_ `.rs` file as input.
    - the processing of that file may result in loading other source files as
      modules
- a rust source file
    - is just _items_ and _attributes_
    - defines a single module, the name of which is set from _outside_ the file.
    - the name can be set by
        1. the crate the file is in
        2. an explicit `mod_item` (???) in a referencing source file
- each crate contains a heirarchy of modules

    crate |-> crate_root module |-> module |-> module |-> module |-> module |->
    module |-> module |-> module

- the top level module ("crate_root") is anonymous from the pov of paths within
  the module
- rust does not _enforce_ a relationship between source files and modules
- a module without a body is loaded from an external file
    - defaults to the name of module with `.rs` extension
- crates that contain an executable contain a `main` function
- a crate taht contains a `main` function _can_ be compiled to be an executable
    - the main function must take no args and return unit `()`
- all functions in module are private unless "exported" with the `pub` prefix

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

- 2 types of "view items"
    1. `extern crate foo;`
    2. `use bar;`
- they don't define new items - they just change other items visiblitiy

#### `extern crate`

- binds an extenal crate to the given identifier
- remember that an external crate is a separate compiled unit - a library file
    - Q: are they statically/dynamically linked by default?

```rust
extern crate foo as foo;
extern crate foo; // shorthand for line above

extern crate foo as bar; // can change name of binding
```

#### use

- shortens paths so you have less typing to do
- NB: does NOT declare linkage depenendency with other crates!!! (extern crate
  does this)

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

- You can re-open a module multiple times. This is very handy if you change the
  "preprocessor directive thingies" that go with it e.g. you can tell rust not
  to compile some bits for certain platforms
- You import a crate with `extern crate some_crate;` Q: what exactly does this
  do? is it like C extern?

    TODO: make a crate that spans multiple files and has multiple modules
