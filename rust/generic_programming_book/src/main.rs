/**
 * Goal:
 *
 * Make the fastest possible multiplication (assuming we can't use built-in `*`)
 *
 */

use std::mem;

fn main() {
    println!("Answer from multiple_1: {}", multiply_1(59, 41));
    // println!("Answer from multiple2: {}", multiply2(59, 41));
    // println!("Answer from multiple3: {}", multiply3(59, 41));
}


// simplest implementation of the egyptian multiplication algorithm
// worst case: same num of iterations as a
fn multiply_1(a: isize, b: isize) -> isize {
    println!("a: {}, b: {}", a, b);

    // stop condition
    if a == 1 { return b; }

    return multiply_1(a - 1, b) + b;
}

// worst case: same num of iterations as smaller of a, b
fn multiply2(mut a: isize, mut b: isize) -> isize {
    println!("a: {}, b: {}", a, b);

    // swap arguments to reduce no. of iterations
    // QUESTION: will rust inline this for us if we put it in a function?
    if a >= b {
        mem::swap(&mut a, &mut b);
        // let tmp = a;
        // a = b;
        // b = tmp;
        // println!("a: {}, b: {}", a, b);
    }

    // stop condition
    if a == 1 {
        return b;
    }

    return multiply2(a - 1, b) + b;
}

fn multiply3(a: isize, b: isize) -> isize {
    if a == 1 {
        return b;
    }
    let mut result = multiply3(half(a), b + b);
    if odd(a) {
        result = result + a;
    }
    return a;
}

/**
 * Helper functions
 *
 */

fn odd(a: isize) -> bool {
   return a % 2 == 1;
}

fn half(a: isize) -> isize {
   return a / 2;
}


#[test]
fn test_swapping_values() {
    let a = &mut 5i;
    let b = &mut 2i;
    mem::swap(a, b);
    assert!(*a == 2);
    assert!(*b == 5);
}
