
// # Attribtes
// * #[] is the syntax that wraps "attributes"
// * #![] sets the attribute for the thing that the line is within, not what is on the following
// line.

// Turn off a bunch of useful warnings that make compiling this dummy code very noisy.
#![allow(dead_code)]
#![allow(unused_variable)]
#![allow(dead_assignment)]
#![allow(unused_mut)]

// main is the entry point of the program because history.
fn main() {
    play_with_rust();
    play_with_loops();
    play_with_strings();
    play_with_vectors();
    play_with_io();
}

// main is the entry point of the program
fn play_with_rust() {

    // FIXME hi
    let x = 5i;
    // x = 6; // COMPILE ERROR: mutating variable not declared as mutable

    let mut y = 4i;
    y = 5; // OK because we declared it as mutable


    // Need to supply a type if rust cannot infer type
    let z: int;

    // We must provide a value before using a variable
    // println!("hello {}", z); // COMPILE ERROR: use of uninitialized variable

    // Placeholder syntax for string interpolation is {} (similar to #{} in ruby)
    println!("hello {}", x);

    // KEY IDEA: The LHS is a full pattern not just a variable name
    let (x, y) = (3i, 5i);

    // The ! indicates that this is a macro not a function call
    // @wycats says this will become @println() soon
    println!("Hello world");

    // Rust is _mostly_ an _expression based_ language. An expression is something that returns a
    // value, a statement does not. There are only 2 kinds of statements, everything else is an
    // expression:
    //
    // 1. Declaration statements (let, anything that is an item declaration, {may be others too})
    // 2. Expression statement (use `;` to throw away return value)
    //
    // Declaration statements do **not** return anything so you cannot do
    // let x = y = 34i;
    //
    // let mut x = 23i; // a statement
    // x = 45;          // an expression that returns `()` the
    //                  // unit type (so return value not very useful)
    //

    // ## ()
    // * pronounced "unit"
    // * is the only valid value for variables of type () (a bit like nil in ruby I guess)
    // * is its own distinct type unlike `null` in C. `null` is a valid value for type `integer` in
    //   C but that would not be the case in Rust because () is a different type to `int`

    // ## ;
    // * turns any expression into a statement by throwing away its return value and returning ()
    //   instead.
    // * Rust's grammar expects "statements to follow statements" so we use `;` to turn expressions
    // into statements to keep it happy


    // `if` is an expression in rust

    let xx:int = if x == 5i { 10i } else { 13i };
    // Notice how the use of `;` will break `if` here because `;` turns the body of the blocks into
    // statements so they don't return anything.
    // let yy : int = if x == 5i { 10i; } else { 13i; }; // Compile error: expecting int and got ()
}

// # 6. Functions

// The most general form of function declaration is:
// fn <func-name>(<arg-name>: <arg-type, ...) -> <return-type> { <func-body> }

// Function args have similar syntax to let statements in how you add type information but you must
// declare types of function args. Rust does not do inference here (seems this was a deliberate
// decision)
// fn foo(x, y) { } // error: expected `:`, found `,`

// You can omit the return type if the function does not return anything
// QUESTION: if i don't return anything does it implictly return anything?  e.g. ()
fn no_args_no_return() { }

// Rust functions can return exactly one value
fn no_args() -> int { 3 + 4 }

// Note this is a compile error because the ; throws away the result so function returns ()
// instead
// fn no_args_1() -> int { 3 + 4; } // error: cannot determine a type for this expression:
                                    // cannot determine the type of this integer; add a suffix to
                                    // specify the type explicitly [E0101]

// You can do explicit returns but it considered bad style
fn explicit_return() -> int {
    return 3 + 4;
}

// You can use the return keyword to do early returns
fn early_return(x: int) -> int {
    if x < 5 { return x }
    3 + 4
}

// # 7. Comments

// // = line comment
// /// = doc comment

/// # Doc comments
///
/// * can contain markdown
///
/// ```
/// println!("including code blocks");
/// ```
fn probably_an_item() {}

// It is a compile error to have a doc comment that is not followed by an "item"

// # 8. Compound Data Types

// There is no magical top-level execution context in rust
// let nope = 34i; // Compile error

fn just_a_wrapper() {
    // xx is a two-tuple of an integer and a string slice
    let xxx = (45i, "hi");

    // same thing with explicit types
    let xx_with_types: (int, &str) = (45, "hi");

    // &str is a "string slice"

    // You can take apart tuples into fields with a destructuring assignment
    // the individual parts of a tuple are fields
    let (x, y, z) = (3i, 4i, 5i);

    // tuples are only equivalent if the arity, types and values match
    let a = (3i, 4i, 5i);
    let b = (6i, 4i, 5i);

    if a == b {
        println!("tuples are equal");
    } else {
        println!("tuples are not equal");
    }

}

// one of the main use cases for tuples is to return multiple values from a function.
fn tupler_returner() -> (int, int) {
    (4i, 6i)
}

// QUESTION: how do extract just one field from a tuple?

// Structs can be outside functions
// QUESTION: Is it true that "items" are what can appear at the top level of a file?
// * Tuple is a "record type" that has an ordering and you extract fields form it using that
//   ordering. Structs have no ordering (similar to mathmetical sets) and you extract fields using
//   names.
// * Struct names are begin with capital and are camel case (unlike functions and variables)
//      QUESTION: what about struct field names?
// * The 'struct' keyword is used to create structs and Tuple-structs

// Meaning #1: 'Point' is the name of the type
// Meaning #2: 'Point' is also the name of the value constructor

struct Point { // Meaning #1
    x: int,
    y: int
}

fn point_player() {
    // struct is like a set, there is no implied ordering of fields
    let place = Point {y: 3i, x: 5i}; // # Meaning #2

    // The mutability of the struct is decided when it is instantiated so you can easily make
    // mutable and immutable versions of the same struct.
    let mut mut_place = Point { x: 0i, y: 7i }; // # Meaning #2

    // You get at fields using dot notation
    println!("{}, {}", place.x, place.y );

    // let Point(newx, newy) = place; // Compiler error (only works for Tuple structs)

    // Rust also has a hybrid struct-tuple called a "Tuple struct"
    // These are a lot like types in Haskell
    // Meaning #1: 'Color' is the name of the type
    // Meaning #2: 'Color' is also the name of the value constructor
    // Meaning #3: 'Color' is also the name of the selector function that can pick fields out of
    //              the Tuple struct

    // Meaning #1
    struct Color (int, int, int);

    struct ThreeDPoint (int, int, int);

    // Note that normally tuples with the same types, values and order are equal but with Tuple
    // structs they are not. Presumably tuple struct annotates the tuple with some type information

    // Meaning #2
    let c = Color(1i, 3i, 5i);
    let pt = ThreeDPoint(1i, 3i, 5i);

    // if c == pt { // error: binary operation `==` cannot be applied to type `point_player::Color`
    //     println!("same");
    // } else {
    //     println!("not the same");
    // }

    // Meaning #3
    let Color(r, g, b) = c;

    // In most cases a struct is more suitable than a tuple struct because you get nice names.
    // The tuple struct can be used to do type aliasing
    // 'Height' is the name of the struct
    // 'Height' is the name of the value constructor
    // 'Height' is the name of the selector function that will pull the value out again.

    struct Height(int);

    let h = Height(10); // construct the type
    // ... do some stuff
    let Height(extracted_h) = h;

    println!("extracted height is {}", extracted_h);

}

// http://doc.rust-lang.org/guide.html#enums
// Enums
// Rust has enums which are a "sum type"

// enum
// * functions as a parameterized type i.e. enum value construtors can take other types as
//   parameters
fn enums_play() {

    enum TrafficLights {
        Red,
        Green,
        Orange
    }

    let tls = Red;

    enum MaybeInt {
        Really(int),
        Missing
    }

    let x = Really(4i);
    let y = Missing;

    // Enums value constructors can take any number of types as arguments
    // The inside of an enum block is in the domain of types
    // * This enum contains value constructors that take only one kind of parameter but enums can
    // be written to use many types i.e. generic enums
    enum MaybeColor {
        Color(int, int, int),
        MissingColor
    }

    let col = Color(3i, 4i, 5i);
    let other_col = MissingColor;
}

// Rush has parameter matching - not in function bodies but using the `match` syntax

fn show_match_syntax() {

    let x = 1i;

    match x {
        // pattern => code-to-execute
        1 => println!("hi"),
        2 => println!("hi"),
        // rust does exhaustiveness checking of match syntax so we can use `_` as a "catch all"
        // match to cover all other cases.
        _ => println!("other")
    }

    enum MaybeColor {
        Color(int, int, int),
        MissingColor
    }

    let c = Color(3i,4i,5i);

    // the pattern will destructure enums automatically. Notice here that `c` is a MaybeColor but
    // we were able to directly check for its value constructor.
    match c {
        Color(_,_,_) => println!("hi"),
        MissingColor => println!("missing")
    }

    // match is an expression not a statement so you can use it in a let statement
    let result = match c {
        Color(x,y,_) => "is a color",
        MissingColor => "is missing"
    };
}

// http://doc.rust-lang.org/guide.html#looping
// Rust has 2 loop constructs
// 1. for
// 2. while
// 3. loop

fn play_with_loops () {
    for x in range(0i, 10i) {
        println!("Iteration: {:d}", x);
    }
    // The general form:
    // for var in expression { code }
    // "expression" must be an iterator
    //      range() is an iterator, the upper bound is not included
    // It is a deliberate choice not to have a for loop because it is really easy to get the end
    // conditions wrong
    // while loops are the correct choice when you do not know how many times you will need to loop
    let mut done = false;
    let mut x = 5u;

    while !done {
        println!("looping, x: {}", x);
        x = x + 1;
        // Rust will warn you if you use unnecessary parens in an if statement!
        if x % 5 == 0 { done = true }
    }

    // Rust has break and continue that work as they do in other languages
    // They can be used in both for and while loops.

    // This is a cleaner rewrite of the loop above that does not need the mutable "done" variable
    // to keep track of when we should stop.
    let mut v = 5u;
    loop {
        println!("looping, v: {}", v);
        v = v + 1;
        if v % 5 == 0 { break }
    }
    // Rust has special syntax for infinite loops
    //
    // loop { }
    //
    // is preferable to
    //
    // while true { }
    //
    // because it gives the compiler more info about what you intend to happen
}

// # Strings
//
// * Strings are variable length data structures
// * There are 2 types
//      1. String
//          * can be made mutable
//          * if mutable can grow and shrink
//          * simple references to fixed size chunk of data in memory
//      2. &str (pronounced "string slice", string literals are this type)
//          * statically allocated
//          * a reference to another string
//          * fixed size, cannot be made mutable
//          * they are objects that can allocate memory
//
// In general all strings:
// * They are not null terminated (so they can contain null bytes)
// * They are streams of UTF-8 encoded bytes


fn play_with_strings() {
    println!("## 11. Strings");

    // This string slice is "statically allocated" so it will be compiled into the binary and will be loaded
    // into memory whenever the binary is.
    // * 's1' is a binding to this chunk of memory.
    // * static strings have a fixed size and cannot be made mutable
    let s1 = "I am compiled into the binary, fixed size, forever immutable";
    println!("{}", s1);


    // String is an "in-memory" string
    // It is garuanteed to be UTF-8
    // It can be mutated, it can grow or shrink
    let mut s2 = "hello".to_string();
    println!("{}", s2);
    s2.push_str(" world");
    println!("{}", s2);

    // So we have 2 types of string, how do we compare them in a typed language?
    // We can convert between the types using to_string()
    let str_slice = "hello";
    let string_object = str_slice.to_string(); // expensive, allocates memory
    let slice_again = string_object.as_slice(); // cheap, no memory allocation

    // Using to_string() allocates memory so is a comparitively expensive operation so prefer using
    // as_slice() to compare strings of different types if possible
}

// # Vectors
//
// Rust has 3 vector types:
//
//  1. Vector Vec<T>
//      * dynamic size
//      * can be made mutable, will allocate memory if necessary
//      * created with `vec!()` macro
//  2. Slice &[T]
//      * a reference to another array
//      * ‘borrowed vector’ == ‘slice’
//  3. Array [T, .. N]
//      * fixed size, cannot add/remove elements
//      * can be made mutable
//
// * All are indexed from 0
//
fn play_with_vectors() {
    println!("## 12. Vectors");

    // Create vectors
    // [] and () are interchangeable in Rust macros, we tend to use [] with vec!
    let x_1 = vec!(2i, 3i, 4i);
    let x_2 = vec![2i, 3i, 4i]; // exactly same as above

    // Create arrays
    // * fixed size, cannot add/remove elements
    // * can be mutable
    // * Type of a_1 is
    //      [int, ..3]
    //   or "an array of ints of length 3"
    let a_1 = [2i, 3i, 4i];
    let nums = [3i, ..20]; // shorthand for array of 20 elements, all initialized to 1

    // Create slices
    let slicey_1 = &[3i, 4i, 5i];
    let slicey_2 = x_1.as_slice();

    // All 3 kinds of list implement the iter() function which returns an iterator that you can use
    // with loops
    for i in x_1.iter() {
        println!["Vector element: {}", i];
    }
    // Aside: i is not available outside the loop construct because Rust is not insane.

    for i in slicey_1.iter() {
        println!["Slice element: {}", i];
    }

    // QUESTION: I don't understand diff between array and array slice ???
}

// Importing modules and crates
// We could import the function so that we can use it unqualified
//      use std::io::stdin;
//      stdin();
// but this could be confusing so it is better to import the module
//      use std::io;
//      io::stdin();
// so it is clear what module each function is from.

fn play_with_io() {
    println!("## 13. IO");

    let input_1 = std::io::stdin().read_line().ok().expect("Failed to read line");
    println!["Input is: {}", input_1];

    let input_2 =
        std::io::stdin()
        .read_line() // returns an IoResult<T> (an optional type) which can either have a thing or not
        .ok() // handle the valid case by doing nothing
        .expect("Failed to read line"); // handle the "missing" case by
                                        // terminating the program with an error
    println!("Second input: {}", input_2);
}


// up to start of 15
