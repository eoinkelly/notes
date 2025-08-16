# Rust

```rust
fn main() {
    // run a macro
    println!("hi there");
    // "hi there" string is statically allocated
}
```

# How do I implement a "class" in rust?

Lets try implementing this simple ruby class in Rust:

```ruby
class Foo
    def initialize(a, b)
        @a = a
        @b = b
    end

    def addtastic
        a + b
    end
end

f = Foo.new(4,6)
puts "Result is: #{f.addtastic}"
```

## Background

Rust provides 4 componound data types

1. tuple
    - an ordered list of a fixed size
    - the structure has no name
    - the elements have no names
    - elements are retrieved by destructuring
2. struct
    - the structure has a name
    - each element has a nane
    - elements are retrieved based on name
    - `struct` with `impl` mimics what we have in ruby as class syntax
3. tuple-struct
    - the structure has a name
    - the elements have no names
    - it is almost always better to use a real struct instead of a tuple-struct
    - the main use case for tuple-structs is to alias one type with another
4. enum
    - a "sum type" i.e. type X is exactly one of the values from the provided
      set
    - can be simple set of values
    - each value can take parameters
    - `match` expression can destructure the values contained in an enum value
    - can be used to implement option type

`=` in rust reads as _take what is on the RHS and bind to the LHS breaking apart
the structure of the RHS as required to make it happen_

```rust
// create a tuple
let x = (2i, 4i, 6i);

// create tuple-struct (named tuple)
struct Color(int, int, int);
struct Point(int, int, int);

let black = Color(0,0,0);
let origin = Point(0,0,0);

// Inches is a tuple that contains one element
struct Inches(int);

let length = Inches(10);

let Inches(inner_int) = length; // unbox the tuple with a destructuring let
```

## Solution

```rust
struct Foo {
    a: int,
    b: int
}

impl Foo {
    fn addtastic(&self) -> int {
        self.a + self.b;
    }
}

let f = Foo { a: 4, b: 6 }
println!("Result is: {}", f.addtastic());
```

### 3 selves

A function within an `impl` block is an instance method if it takes one of

1. `self`
2. `&self`
    - most common
3. `mut &self`

as its first parameter. When you call `thing.do_something(a,b);` it actually
passes a reference to `thing` as the first parameter to `do_something`

QUESTION: what is the difference between them? it seems like the `.` syntax is
just sugar for calling functions and the normal rules of references and
borrowing apply (i don't yet understand those rules so cannot be sure)

## Extras

- Can I have multiple `impl` blocks for a single `struct`?
    - Yes.
- Can I override methods defined in a previous `impl` block?
    - No - compiler error.

# How can I implement ruby block syntax in rust?

```ruby
def simple_wrap(a, b, &block)
    puts "Header: #{a}"
    yield(3)
    puts "Footer: #{b}"
end

# interestingly the method defn above is not enough info to implement this in
# rust - I also need to see an implementation

simple_wrap(4, 5) do |x|
    puts "Body: #{x}"
end

```

```rust
fn simple_wrap(a: int, b: int, f: |int| ) {
    println!("Header: {}", a);
    f(3);
    println!("Footer: {}", b);
}

simple_wrap(4i, 5i, |x| { println!("Body: {}", x); }
```

TODO: finish this

# How do I do ruby class inheritance in rust

```ruby
class Bar
    def some_fun
    end
end

class Foo < Bar
    def thing_from_foo
    end
end
```

    TODO: do this

# How do I do ruby module inclusion in Rust?

```ruby
module Bar
    def some_fun
    end
end

class Foo
    include Bar
    def thing_from_foo
    end
end
```

    TODO: do this

# How do I do ruby module extension in Rust?

```ruby
module Bar
    def some_fun
    end
end

class Foo
    extend Bar
    def thing_from_foo
    end
end
```

    TODO: do this
