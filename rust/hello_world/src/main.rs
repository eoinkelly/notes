
// #[] are attributes
// they only seem to apply to the thing (function, struct etc.) that hey appear before in the code.
// Q: how do i disable these for the whole file?
#[allow(dead_code)]
#[allow(unused_variable)]
#[allow(dead_assignment)]
// main is the entry point of the program
fn main() {

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
    // 1. Declaration statements (let, {there are others})
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

// up to
// http://doc.rust-lang.org/guide.html#structs
