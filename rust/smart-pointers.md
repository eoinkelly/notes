# Smart pointers

## regular references

* a rust reference `&` is a simple pointer.
* it contains the address of the data it points to
* within the rust typesystem, it borrows the value it points at (whether immutably or mutably)

## smart pointers

* they _own_ the data they point to
* they can enforce rules for data access
* can have extra capabilities
* can have metadata
* examples
    * `String`
        * ensures written bytes are always UTF-8 compatible
        * has allocated capacity as metadata
    * `Vec<T>`
        * has allocated capacity as metadata
* usually implemented using structs
* a smart pointer is a struct which implements the `Deref` and `Drop` traits
* `Deref`
    * allows an instance of the struct to behave like a reference (so you can write code which takes either real references or smart pointers)
    * lets you customise the behaviour of the `*` deref operator (the operator that lets you get from a ref to it's pointed at data)
    * `*x` desugars to `*(x.deref())` - the "real" `*` operator is called on the reference returned by `x.deref()`
        * only one layer of desugaring happens - the desugared is not desugared (would be infinite loop)
    * without the `Deref` trait, rust can only dereferences `&` things. With the trait, it can dereference anything which implements it
    * `deref()` teaches rust how to get the actual "dumb pointer" reference out of your fancy stuct
* `Drop`
    * allows you to customise the code that is run when a pointer goes out of scope
* many libs include their own smart pointers
* smart pointers in `std`:
    * `Box<T>`
        * for explicitly allocating values on the heap
    * `Rc<T>`
        * reference counting type
        * enables multiple ownership
    * `Ref<T>`, `RefMut<T>`, `RefCell<T>`
        * enforces borrowing rules at runtime instead of compile time

### Box<T>

* lets you store data on the heap, with a pointer to it on the stack
* not much perf overhead, no extra capabilities
* use when indirection is the only thing you need
* use cases
    1. you have a large chunk of data. You want to transfer ownership but ensure the large data won't be copied around on the stack
    1. You want to own a value. You care only that it implements a particular trait and not what it's actual type is
    1. You have a type whose size can't be known at compile time and you want to use a value of that type in a context which requires an exact size
        * called a _Trait object_

```rust
// will not compile because rust can't figure how much space a List would need on the stack
// enum List {
//     Cons(i32, List),
//     Nil,
// }

// rust will compile this because it knows how much stack space each List takes up (rust doesn't care about heap space)
#[derive(Debug)]
enum List {
    Cons(i32, Box<List>),
    Nil,
}

struct MyBox<T> {
    thing: T
}

impl<T> MyBox<T> {
    fn new(x: T) -> MyBox<T> {
        MyBox { thing: x }
    }
}

use std::ops::Deref;

// Deref
impl<T> Deref for MyBox<T> {
    type Target = T; // define an "Associated type"

    // deref() returns a "dumb pointer" reference to the actual data
    fn deref(&self) -> &Self::Target {
        &self.thing
    }
}

fn box_play() {
    let b = Box::new(55); // b comes into scope
    println!("b = {:?}", b);

    let c = *b + 3; // use the dereference operator to get at the data "pointed to" by the Box
    // do i need to be careful dereferencing data in a box so that it's not copied onto the stack?
    println!("c = {:?}", c);

    let list = List::Cons(3, Box::new(List::Cons(2, Box::new(List::Cons(1, Box::new(List::Nil))))));
    println!("{:?}", list);

    let x = 5;
    let y = MyBox::new(x);
    assert_eq!(5, x);
    assert_eq!(5, *y);

} // b is deallocated, runs the Box Drop code to drop both the heap and the stack parts of b
```
