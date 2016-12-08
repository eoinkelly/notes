#[macro_use] extern crate t_bang;
use t_bang::*;

extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;

fn main() {
    println!("A silly rust program to dump types");

    let x = 33.4;
    pt!(x);

    let s = "foo";
    pt!(s);
    pt!(s.to_string());

    let heap_string = "something".to_string();
    pt!(heap_string);

    let s = "u͔n͈̰̎i̙̮͚̦c͚̉o̼̩̰͗d͔̆̓ͥé";

    // iterate across as bytes
    for x in s.as_bytes() {
        println!("{} has type {}", x, t!(x));
    }

    // iterate across as unicode codepoints
    for x in s.chars() {
        println!("{} has type {}", x, t!(x));
    }

    // iterate across as unicode grapheme clusters
    for x in UnicodeSegmentation::graphemes(s, true) {
        println!("{} has type {}", x, t!(x));
    }
}
