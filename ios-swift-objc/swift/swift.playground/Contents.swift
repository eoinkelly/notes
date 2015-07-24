/*

* type safe
* type names begin with uppercase
* requires explicit return statements

*/

// console logging stuff
print("hello world")
print("hello world", appendNewline: false)
var someThing = 12
print("someThing is: \(someThing) at current point")
// notice: no explicit type coercion required so it doesn't introduce a type
// dependency the way C does
// Q: what method on the object does \() call


/*:
# Creating primitive variables

* `let` creates a constant (value can be set at runtime but can only be set once)
* `var` creates a mutable variable

*/

let noChanges = "hello"
//noChanges = "nope" // compiler error

var changeMe = "hello"
changeMe = "yep" // works fine

// multi-line initializations and declarations works
let a = 1, b = 2, c = 3, d:Float, e:Int

// + is overloaded to do addition and concatenate strings
let workgString = "i am" + " long"
let finalResult = 23 + 5


// multi-line strings
let manyLines = "hello " +
                "I am a multi-line" +
                " string "

// statements must be separated by semicolons if on same line
// end of line semicolons are optional
let out1 = 23; print("hello \(out1)")


// type annotations
let t1 = 12 // compiler can infer type from value
let t2: Double // explicit declaration because no value provided (notice the spacing)
// spacing is <var-name>: <type-name>
let t3, t4, t5: String // multiple declarations with a single explicit type


/* 
# Integers

Has 4 sizes of signed int

Int8
Int16
Int32
Int64

Has 4 sizes of unsigned int

UInt8
UInt16
UInt32
UInt64

* The `Int` type has the same size as the current platforms native word size i.e.
  it will be 32bit when run on 32 bit devices and 64 on 64bit devices

* When you need to do efficient computations with integers consider using Int 
  instead of UInt to minimize the amount of type juggling the compiler has to do.

*/

let small: UInt8

// You can use the max and min *properties* to introspect the integer type bounds
UInt16.max
UInt16.min
Int16.max
Int16.min

// My laptop has 64bit word size
Int64.max
Int.max
Int64.min
Int.min


let veryLargeNum = 1_234_456_554 // can use _ to make large numbers readable
let someBinaryNum = 0b001111001010
let someOctalNum = 0o775
let someHexNum = 0x1222eef

// swift will NOT automatically convert integer types if they overflow
//let nope = UInt8.max + 1
//let nope: UInt16 = UInt8.max + 1
let yep: UInt16 = UInt16(UInt8.max) + 1 // Use version of the UInt16() constructor that takes an UInt8

// Numeric literals are only given a type when the compiler evaluates them so
// they will automatically take on the most appropriate type the compiler can find
// e.g.

let pi: Double = 3.14
let newPi = pi + 3 // works as 3 becomes a Double when evaluated

let fiddleFactor: Int = 3
//let newPi2 = pi + fiddleFactor // error because cannot add Int and Double



/*:
# Types

* type names are uppercase (like Haskell)
Primitive types

* Double
* Float
* String
* Int
* Bool

Collection types

* Array
* Set
* Dictionary

Fancy types

* Tuples
* Optionals

*/
var str = "Hello, playground" // type inferred as String

let x = 23 // type infered as Int
x.dynamicType // => Int.Type
let x_2: Double = 23 // set explicit type
x_2.dynamicType // => Double.Type

/*
Introspecting types

dynamicType message returns a reference to the
*/

class mySimpleClass {
    let thing = 12
}
let mySimpleInstance = mySimpleClass()
mySimpleInstance.dynamicType
mySimpleClass.self
mySimpleClass.self == mySimpleInstance.dynamicType

23.dynamicType
Int.self
//23.dynamicType == Int.self // error
