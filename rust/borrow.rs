
fn main() {
    // v is a reference to a mutable vector
    // variables have ownership, not scopes
    // v "owns" the vector
    let mut v = vec![];

    // mutate the vector through v
    v.push("Hello");

    // x is an immutable borrow of v
    // v is lending its "ownership" to x
    // => so & on the RHS implies that a borrow is happening
    // anywhere you see & we are "aliasing" i.e have two variables pointing to same part of memory
    let x = &v[0];

    // we have loaned v out to x (as read only) so
    // we cannot mutate v until that loan is returned
    v.push("world");

    println!("{}", x);
}
