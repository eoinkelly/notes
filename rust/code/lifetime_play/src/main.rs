use std::fmt::Display;

fn main() {
    // example_0();
    // example_1();
    // example_2();
    example_4();
    // show(33, "foo");
}

fn show<T: Display, U: Display>(a: T, b: U) {
    println!("{}", a);
    println!("{}", b);
}

// s1 has exactly the same lifetime as s2
fn example_0() {
    let s1 = String::from("foofoo");
    let s2 = String::from("barbar");
    let result = longest(s1.as_str(), s2.as_str());
    println!("String 1: {}", s1);
    println!("String 2: {}", s2);
    println!("Result is: {}", result);
}

// s1 has a longer lifetime than s2
fn example_1() {
    let s1 = String::from("foofoo");

    {
        let s2 = String::from("barbar");
        // here the 'a lifetime becomes min(concrete-lifetime-of-s1, concrete-lifetime-of-s2)
        let result = longest(s1.as_str(), s2.as_str());
        println!("String 1: {}", s1);
        println!("String 2: {}", s2);
        println!("Result is: {}", result);
    }
}

fn example_2() {

    // let s1 = String::from("foofoo");
    // let result: &str;
    //
    // {
    //     let s2 = String::from("barbar");
    //     // here the 'a lifetime becomes min(concrete-lifetime-of-s1, concrete-lifetime-of-s2)
    //     result = longest(s1.as_str(), s2.as_str());
    //     println!("String 1: {}", s1);
    //     println!("String 2: {}", s2);
    //     println!("Result is: {}", result);
    // }
}

// This version does not compile because rustc can't tell at compile time
// whether the return value will be a reference to x or y (x and y could have
// different lifetimes)
// fn longest2(x: &str, y: &str) -> &str {
//     if x.len() > y.len() {
//         x
//     } else {
//         y
//     }
// }

fn example_4() {
    // works
    // let x = String::from("hello");
    // {
    //     let y = String::from("goodbye");
    //     let z = longest(&x, &y);
    //     println!("Z: {}", z)
    // }

    // works
    // let x = String::from("hello");
    // let y;
    // let z;
    // {
    //     y = String::from("goodbye");
    //     z = longest(&x, &y);
    // }
    // println!("Z: {}", z)

    // breaks
    // let x = String::from("hello");
    // let z;
    // {
    //     let y = String::from("goodbye");
    //     z = longest(&x, &y);
    // }
    // println!("Z: {}", z)

}

// These lifetime annotations do not require x and y to have exactly the same lifetime
// I think rust will take the shortest of the concrete lifetimes of x and y and assign that to 'a
// We are tlling the compiler "there must exist a lifetime in the set of
// lifetimes that are passed in from the params that satisfies this constraint
// and you can assume return value lives for that length"
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() {
        x
    } else {
        y
    }
}

fn bad_idea() -> &str {
    let x = String::from("blah");
    &x
}