fn main() {
    let input = 3;
    let answer = fib1(input);
    println!("Fib of {} is {}", input, answer);
}

fn fib1(n: int) -> int {
    println!("n: {}", n);
    if n < 2 {
    // if n == 0 {
        n
    } else {
        fib1(n - 1) + fib1(n - 2)
    }
}
