
// main is the entry point of the program
fn main() {

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
    println!("Hello world");


    // Rust is mostly an _expression based_ language There are only 2 kinds of statements,
    // everything else is an expression.
    //
    // 1. Declaration statements (let, {there are others})
    // 2. Expression statement (use `;` to throw away return value)
    //
    // An expression is something that returns a value, a statement does not.
    // Declaration statements do **not** return anything so you cannot do
    // let x = y = 34i;
    //
    // let mut x = 23i; // a statement
    // x = 45;          // an expression that returns `()` the unit type (so return value not very useful)
    //

    // ()
    // * pronounced "unit"
    // * is the only valid value for variables of type () (a bit like nil in ruby I guess)
    // * is its own distinct type unlike `null` in C. `null` is a valid value for type `integer` in
    //   C but that would not be the case in Rust because () is a different type to `int`

    // ;
    // * turns any expression into a statement by throwing away its return value and returning ()
    //   instead.
    // * Rust's grammar expects "statements to follow statements" so we use `;` to turn expressions
    // into statements to keep it happy


    // if is an expression in rust

    let xx:int = if x == 5i { 10i } else { 13i };
    // let yy : int = if x == 5i { 10i; } else { 13i; }; // Compile error: expecting int and got ()

    // Up to start chap 6 http://doc.rust-lang.org/guide.html
}

