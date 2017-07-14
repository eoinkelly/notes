use std::fmt::Display;

fn main() {
    example_0();
    example_1();
    example_2();
    show(33, "foo");
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

fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() { x } else { y }
}
