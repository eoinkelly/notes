fn main() {
    println!("Hello, world!");

    // struct Foo {
    //     a: int,
    //     b: int
    // }
    //
    // impl Foo {
    //
    //     // Static method (because it does not take self|&self|mut &self as first paramater)
    //     // Invoked as Foo::new(...)
    //     fn new(a: int, b: int) -> Foo {
    //         return Foo {
    //             a: a,
    //             b: b
    //         }
    //     }
    //
    //     // Instance method
    //     // Invoked as: foo.addtastic(...)
    //
    //     fn addtastic(&self) -> int {
    //         return self.a + self.b;
    //     }
    //
    //     fn add_with_other(&mut self, c: int) -> int {
    //         self.a = self.a + 1;
    //         return self.a + self.b + c;
    //     }
    // }
    //
    // let f2 = Foo::new(2i,4i);
    // println!("Result is {}", f2.addtastic());
    //
    // // so the ref.func(..) syntax passes a reference to self
    // println!("Result is {}", f2.add_with_other(5i));


    // fn twice(x: int, f: |int| -> int) -> int {
    //     f(x) + f(x)
    // }
    //
    // fn square(x: int) -> int {
    //     x * x
    // }
    //
    // println!("Twice result: {}", twice(5i, |x| { x * x }));
    // println!("Twice result: {}", twice(5i, square)); // can also pass named function where closure is expected
    //
    // fn simple_wrap(a: int, b: int, f: |int| ) {
    //     println!("Header: {}", a);
    //     f(3);
    //     println!("Footer: {}", b);
    // }
    //
    // simple_wrap(4i, 5i, |x| { println!("Body: {}", x); });

    /**
     * Change ownership
     *
     */

    fn give() {
        // Create veccy so we own it
        // veccy is allocated directly on the stack
        // The contents of veccy do get put on the heap but veccy itself is on the stack
        let mut veccy = Vec::new();
        veccy.push(1);
        veccy.push(2);

        // Note that the semantices of ownership transfer are clear both here and in the function
        // declaration of take()
        take(veccy); // move veccy into the take function

        // Illegal to use veccy here - veccy has been freed now because take() finished!!!
        // veccy.push(3); // Complile error about veccy being moved
    }

    // expects to get ownership of the reference it is given
    fn take(v: Vec<int>) {
        println!("A vector I own: {}", v);
    }

    give();

    /**
     * Shared borrow
     *
     */

    fn lender() {
        // Create a vector on the stack
        let mut veccy = Vec::new();
        veccy.push(4);
        veccy.push(6);

        // Notice that whether we are lending/borrowing is visible both here in the calling code
        // and in the called function
        user(&veccy); // this actually creates a new pointer to veccy in the stack and gives that out
        veccy.push(7);
        println!("A vector that still exists {}", veccy);
    }

    // expects to borrow an immutable reference
    fn user(v: &Vec<int>) {
        println!("A vector I have borrowed and cannot change: {}", v);
    }

    lender();

    /*
     * Mutable shared borrow
     *
     */

    // ???
}

#[test]
fn tests_work() {
    assert_eq!(1i, 1i);
}

#[test]
fn tuple_structs_with_same_values_are_not_equal() {
    struct Color(int, int, int);
    struct Point(int, int, int);

    let black = Color(0,0,0);
    let origin = Point(0,0,0);

    // assert_eq!(black, origin);
}
