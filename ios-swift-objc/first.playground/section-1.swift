import Cocoa

// var creates variables
var iCanBeChanged = "Hello, playground"
iCanBeChanged = "something else"

// let creates constants
//let iCannotBeChanged: String // No initialization =>  ERRROR
let iCannotBeChanged: String = "hi"

println("I print stuff to console")

let explicitString: String = "hi there"

let anImplicitInteger = 34
let anImplicitDouble = 45.6
let explicitDoubleWithInit: Double = 34

// Explicit type syntax
// let|var variableName: Type
// Notice that types are uppercase like Haskell
let anotherNumber: Double = 56

// \() converts other things to strings
// QUESTION: what method does it call?
var s1 = "hi there \(anImplicitInteger) this is a test"

// + will concatenate strings
let s2 = "hello " + String(anotherNumber)

// + will also add numbers
4 + 5

// There is no explicit conversion between types
//aNumber + anotherNumber // ERROR

let x = 12
let y = 12.4
//x + y // ERROR: no implicit conversion of types
Double(x) + y

let iAmAnArray = [2, 3, 4, 88]
let iAmDict = [
    "a_key": "some_value",
    "another_key": "some other value"
]