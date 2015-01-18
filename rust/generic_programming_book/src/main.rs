fn main() {
    // println!("Answer: {}", multiply1(59, 41));
    println!("Answer: {}", multiply2(59, 41));
}

/**
 * Make the fastest possible multiplication (assuming we can't use built-in `*`)
 *
 */

// simplest implementation of the egyptian multiplication algorithm
// worst case: same num of iterations as a
fn multiply1(a: int, b: int) -> int {
    println!("a: {}, b: {}", a, b);

    // stop condition
    if a == 1 { return b; }

    return multiply1(a - 1, b) + b;
}

// worst case: same num of iterations as smaller of a, b
fn multiply2(mut a: int, mut b: int) -> int {
    println!("a: {}, b: {}", a, b);

    // swap arguments to reduce no. of iterations
    if a >= b {
        let tmp = a;
        a = b;
        b = tmp;
        println!("a: {}, b: {}", a, b);
    }

    // stop condition
    if a == 1 {
        return b;
    }

    return multiply2(a - 1, b) + b;
}

fn multiply3(a: int, b: int) -> int {
    if a == 1 {
        return b;
    }
    let mut result = multiply3(half(a), b + b);
    if odd(a) {
        result = result + a;
    }
    return a;
}

fn odd(a: int) -> bool {
   return a % 2 == 1;
}

fn half(a: int) -> int {
   return a / 2;
}
