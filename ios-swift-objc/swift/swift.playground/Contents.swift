import Foundation
import UIKit

/*:
# Swift Programming Language Guide (starting at "Language Guide" in swift book)

* swift is type safe
* type names begin with uppercase
* end of line semicolons are optional
* requires explicit return statements

*/

/*:
# Printing to stdout and stderr

*/

// printing values to stdout is wonderfully boring
print("hello world") // adds trailing newline as default terminator
print("hello world", terminator: "") // specify a termintor
print("")
print("hello world", terminator: "X")
print("")

// you can print multiple values separated by "separator"
let someYear = "2015"
let someMonth = "12"
let someDay = "01"
print(someYear, someMonth, someDay, separator: "-", terminator: "\n\n")

// Create an example struct
struct ForDumping {
    let a = "hello"
    let b = 44
    let c = ["a", "b", "hello"]
}
let fdp = ForDumping()

// debugPrint writes a single line debug output to console
debugPrint(fdp)
// "ForDumping(a: "hello", b: 44, c: ["a", "b", "hello"])\n"

// * dump() 
// * writes formatted and indented output to the console
// * comes from the swift standard library
dump(fdp) // dump an objects contents using its "mirror"
// ▿ ForDumping
//   - a: hello
//   - b: 44
//   ▿ c: 3 elements
//      - [0]: a
//      - [1]: b
//      - [2]: hello

// variable interpolation is straight-forward
let printyThing = 33.4
print("someThing is: \(printyThing) at current point")

// Note: no explicit type coercion required so it doesn't introduce a type dependency the way C does.

// QUESTION: how does swift coerce types into strings? 
// QUESTION: how to print to stderr

/*:
# Constant & Variable declaration

*/

// let makes immutable values (constants)
let someThing = 12

// constants do not have to be assigned on the same line it is declared
let anotherThing: Int
anotherThing = 33

var variableThing: Int
variableThing = 33
variableThing = 44 // can be re-assigned

// multiple initializations and declarations per line are fine
let a = 1, b = 2, c = 3, d:Float, e:Int

// statements must be separated by semicolons if on same line
let out1 = 23; print("hello \(out1)") // playgrounds don't show multiple statements per line

// swift has nice type annotation but it sometimes needs help
let t1 = 12 // compiler can infer type from value
let t2: Double // explicit declaration because no value provided (notice the spacing is <var-name>: <type-name>
let t3, t4, t5: String // multiple declarations with a single explicit type

// use backticks to create variable names that are swift keywords
let `let` = "terrible idea but will compile"

/*:
# Types

* type names are uppercase (like Haskell)

Primitive types

* Float
* Double
* Float80
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

# Integers

Has 4 sizes of signed integer

Int8
Int16
Int32
Int64

Has 4 sizes of unsigned integer

UInt8
UInt16
UInt32
UInt64

* note that signed ints are the more common so their type name is unprefixed
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

// The Int type has boundaries set by the word size of the machine it is compiled for 
// e.g. my laptop has 64bit word size
Int64.max
Int.max
Int64.min
Int.min

/*:
Swift has three floating point types

* Float (? bits)
* Double (? bits)
* Float80 (80 bit float)

*/

let defaultFloat = Float()      // defaults to 0
let defaultDouble = Double()    // defaults to 0
let defaultFloat80 = Float80()  // defaults to 0.0

// Double is used when swift infers a type e.g.
let someFloatyVal = 3.1459
someFloatyVal.dynamicType

let definitelyFloaty: Float = 3.14
definitelyFloaty.dynamicType

let bigbigfloat: Float80 = 3.1459 // 80 bit number
bigbigfloat.dynamicType


/*:
### representing numbers
*/

// numbers are decimal by default

// use _ to make large numbers readable
let veryLargeNum = 1_234_456_554

let someBinaryNum = 0b001111001010
let someOctalNum = 0o775 // hooray for octal not just being a 0 prefix!
let someHexNum = 0xbeef

// swift will NOT automatically convert integer types if they overflow
//let nope = UInt8.max + 1
//let nope: UInt16 = UInt8.max + 1
let yep = UInt16(UInt8.max) + 1 // Use version of the UInt16() constructor that takes an UInt8

// Numeric literals are only given a type when the compiler evaluates them so
// they will automatically take on the most appropriate type the compiler can find
// e.g.

let pi: Double = 3.14
let newPi = pi + 3 // works as 3 becomes a Double when evaluated

let fiddleFactor: Int = 3
//let newPi2 = pi + fiddleFactor // error because cannot add Int and Double


// hexedecimal numbers can have an power of 2 exponent 
// this does not work on decimal numbers
let thing = 0xFp2 // 0xF = 15, p2 = "* 2^2", 15 * 2^2, => 60
//let foo = 10p3 // error

var str = "Hello, playground" // type inferred as String

let x = 23 // type infered as Int
x.dynamicType

let x_2: Double = 23 // set explicit type
x_2.dynamicType

/*:
# Strings
*/

let string1 = "foo"
let string2: String
let string3 = String()
let string4 = String(42) // String has an initializer that will convert from an Int
let emptyString = ""

emptyString.isEmpty
string1.characters

// + is overloaded to do addition and concatenate strings
let workgString = "i am" + " long"

// multi-line strings are sweet
let manyLines = "hello " +
                "I am a multi-line" +
                " string "

//  Aside: trimming strings
extension String
{
    func trim() -> String {
        // self is not required here as stringByTrimmingCharactersInSet will be looked up on the String instance by default
        return self.stringByTrimmingCharactersInSet(NSCharacterSet.whitespaceAndNewlineCharacterSet())
    }
}

"\n    foo  \n".trim()

/*:
# Collection types

*/

let array1 = Array<Int>()
let array2 = [Int]() // shorthand, prefered syntax

let dict1 = Dictionary<String,String>()
let dict2 = [String:String]() // shorthand, preferred syntax

// Array.append(_:)
var array3 = [String]()
array3.append("new thing")
array3.append("another thing")

/*:
# typealias

* lets you alias one type with another
* makes code more readable

*/
typealias AudioSample = UInt16
let reading_1: AudioSample = 44
var reading_2: AudioSample = 44

/*:
# Optionals

*/

var optionalInt1: Int?
var optionalArray: [Int]?
var arrayOfOptionals: [Int?]

let reading1: Float? = 3
let reading2: Float? = 4
let reading3: Float? = 5

// ways of unwrapping the
let averageReading1 = (reading1! + reading2! + reading3!) / 3
let averageReading2 = ((reading1 ?? 0) + (reading2 ?? 0) + (reading3 ?? 0)) / 3

/*:
# Boolean

* Bool is the boolean type
* can be true|false
* swift has no notion of truthy or falsy

*/

let isHappy = true
let isSad = false
let isSure: Bool

true.dynamicType

// swift has no notion of truthy
let someCounter = 1
//if someCounter { print("counting") } // Type 'Int' does not conform to protocol 'BooleanType'
if someCounter == 1 { print("counting") }

func testThings() -> Bool {
    return true
}

if testThings() {
    print("test passed")
}


/*:
## Tuples

* compound types
* the exact order of types in the tuple make up its type so it provides more type safety than an array
* tuple parts can be retrieved by index
* tuple parts can be labeled and retrieved by label
* decompose with a pattern matching assignment
    * use _ as name for anything you want to ignore
* useful as return values of functions

*/

let httpResponse_1 = (44, "body content")
let httpResponse_2 = (code: 44, content: "body content")

httpResponse_1.0
httpResponse_1.1

httpResponse_2.0
httpResponse_2.1
httpResponse_2.code
httpResponse_2.content

// can decompose the tuple using pattern matching
let (code, content) = httpResponse_1
let (statusCode, _) = httpResponse_1 // _ ignores


/*:
# Control structures

*/
let maxThing = 4

let range1 = 0..<maxThing

for i in range1 {
    print("hi \(i)")
}

let things = ["hello", "there", "boyo"]

for localThing in things {
    // thing only exists within the for loop
    print(localThing)
}
// print(localThing) // Use of unresolved identifer 'localThing'

// use enumerate() to get a tuple of the item and an index
for (localThing2, i) in things.enumerate() {
    print(i)
    print(localThing2)
}

/*
## Aside: Introspecting types

dynamicType message returns a reference to the ????

*/

class MySimpleClass {
    let thing = 12
}

let mySimpleInstance = MySimpleClass()
mySimpleInstance.dynamicType
MySimpleClass.self
MySimpleClass.self == mySimpleInstance.dynamicType

23.dynamicType
Int.self
//23.dynamicType == Int.self // error

/*:
# Classes and structs

* classes and structs are *very* similar in swift
* features in common between classes and structs
    * properties
    * methods
    * use subscripts to access property values
        * subscripts allow you to access properties on a type using `[]` notation
        * examples: array foo[0], hash foo['key']
    * initializers to setup initial state
    * extensions: be extended to expand functionality beyond a default implementation
    * protocols: conform to protocols to provide a standard set of functionality
* class only features
    * classes are reference types (the reference is copied when making a copy)
    * can inherit from other classes
    * gets only one initializer
    * copied by reference
    * can type cast and check type at runtime
    * reference counting allows for more than one reference to a class 
        * ??? does that imply that structs can only have one reference at a time?
    * can have custom deinitializers (destructors I guess) to do custom cleanup
* struct only features
    * structs are value types (the entire struct is copied when making a copy)
    * a `Foo` Struct gets two initializers by default
        1. The same one as classes get: `Foo()`
        2. An initializer that thakes each of the elements in the struct as named parameter
            * You could make this yourself for classes
            * This is called the "memberwise initializer"

* String, Array, Dictionary are all implemented as structs in Swift
*/

class ParentyPoint {
    var type = "hi" //: String
}

// this complains that the class has no intializers ???
//class OtherParentyPoint {
//    var type: String
//}

class ClassyPoint: ParentyPoint {
    var x = 0
    var y = 0
}

struct StructyPoint {
    var x = 0
    var y = 0
}

var myClassyPoint = ClassyPoint()
myClassyPoint.x = 22
myClassyPoint.y = 44
myClassyPoint.type

var myStructyPoint = StructyPoint(x: 22, y: 44)
myStructyPoint.x
myStructyPoint.y

var sp1 = StructyPoint(x: 10, y: 12)
var sp2 = StructyPoint(x: 10, y: 12)
var cp1 = ClassyPoint()
cp1.x = 10
cp1.y = 12
var cp2 = ClassyPoint()
cp2.x = 10
cp2.y = 12

//sp1 == sp2

/*
## Swift identity operators

=== and !== compare whether the operands refer to exactly the same
instance of a class or not
*/

cp1 === cp2


/*:
# Access control

You can assign specific access levels to 
* individual types
    * classes
    * structures
    * enumerations
* properties
* methods
* initializers
* subscripts

Protocols can be restricted to a certain context, as can global constants, variables, and functions.
    HOW ???

* anything from the list above that can have access control specified is an "entity" in docs

Swift access controls are built around the ideas of "module" and "source file"

* module
    * a single unit of code distribution
    * an appliation or framework
    * a thing than can be used with `import` keyword
    * each build target in XCode is a separate module
* source file
    * a single swift source code file
    * is always within some module (app or framework)
    * it is common to define each type in a separeate source file but a single source file
      can contain multiple type definitions

Swift has 3 levels of access

1. public
    * used for the public interface of a framework
    * allows the entity to be used
        * in any source file in the same module
        * in any source file of another module that imports the current module
2. internal aka "module private"
    * allows the entity to be used in any source file in its defining module but nowhere else
    * internal is the default so you don't see it written in source code a lot
3. private aka "file private"
    * only allows the entity to be used in the same source file it was defined in

WARNING: access is scoped by the _file_ not by the code block e.g. private within a class definition does not make the entity private to that class!!!

* swift provides default levels of access control
    * all entities default to "internal" access unless you specify

> a public type defaults to having internal members, not public members. If you want a type member to be public, you must explicitly mark it as such

*/

/*:
## Aside: regular expressions

* swift does not have built-in regex

Ways to do regex in swift

1. Use NSRegularExpression directly
1. Create a swift wrapper around NSRegularExpression
    * https://gist.github.com/ningsuhen/dc6e589be7f5a41e7794
1. Call the Foundation method rangeOfString:options: with RegularExpressionSearch as an option
1. Use a library that provides it
    * http://www.dollarswift.org/#dollar-and-cent (lodash for swift, includes a Regex type which wraps around NSRegularExpression
        * https://github.com/ankurp/Dollar.swift/blob/master/Cent/Cent/Regex.swift
        * it provides matching, escaping, testing
*/

let regexContent = "this is some stuff"
let pattern = "uff$"

// returns nil if there was no match
// returns a swift Range if there was a match e.g. 15..<18
regexContent.rangeOfString(pattern, options: .RegularExpressionSearch)

/*:
# Availability checks

* The * at the end is required
    * it mean "any other platform"
    * it specifies that the if branch will be executed on the minimum deployment target setin the target settings
*/

// are only available in if & guard clauses
// #available(OSX 10.9, *) // compile error

// if #available(platformname version, platformname version, *)
// * the `*` means "require minimum deployment target for other platforms"
// it calls out that there is potential control flow here

// annotate a method to say that it only makes sense on certian platform versions. this
// allows the compiler to verify that this function is called from within an #available test
@available(OSX 10.9, *)
func someFuncThatOnlyWorksOnTenNine() {

}

if #available(OSX 10.9, *) {
    // this branch will be taken if 
    // 1. are on OSX 10.9 or leter
    // 2. are on any other platform!!!
    // e.g. this branch will be executed on any tvOS and iOS that this code builds for
    print("we got mac!")
    someFuncThatOnlyWorksOnTenNine()
} else {
    print("not on mac")
}



/*: 
# Guard clauses

* executes statements based on the _boolean_ value of an expression
* provides nice syntax for doing pre-condition checks in functions
* guard clauses may not fall through! they must contain a return or break

guard TEST else {
  FAILURE_CASE
}

is sugar syntax for

if RISKY_THING {
} else {
  FAILURE_CATCHING_CODE
}

but if expressions require to return true|false and assignments
*/


func doStuff(things: [String: String]) -> Bool {
    guard let name = things["name"] else {
        print("no name found")
        return false
    }

    guard (true && true) else {
        print("blah")
        return false
    }

    // without a guard you need this cumbersome thing to do the same
    // Q: why did they not have an unless let foo = ... { ... }
    if let age = things["age"] {
    } else {
        print("no age found")
        return false
    }

    print("\(name) is the name")
    return true
}

//let things: Dictionary<String: String>
let manyThings: [String: String]
manyThings = ["name": "Eoin", "age": "thirty-six"]

var maybeBlah = manyThings["name"]
maybeBlah.dynamicType

doStuff(manyThings)


/*:
# Functions

* the return type can be implictly Void
* Void is alias for () (the empty tuple
* => swift functions ALWAYS return a value

*/

// explict return Void
func doAThing1(a: Int, b: Int, c:Int) -> Void {
    print("hi there")
}

// explict return empty tuple () which is same as Void
func doAThing2(a: Int, b: Int, c:Int) -> () {
    print("hi there")
}

// implict return Void
func doAThing3(a: Int, b: Int, c:Int) {
    print("hi there")
}

// Void.dynamicType
().dynamicType

// doAThing(3, 4, 5) // compiler error
doAThing1(3, b: 4, c: 5) // works

// Swift has pretty great support for function returning multiple values
// The tuple type is used which allows you to specify the name and type of each part of the tuple in the function signature
// swift's tuples that have named parts are great here!

func makeHttpRequest(url: String) -> (statusCode: Int, content: String) {
    // ...
    return (statusCode: 400, content: "bad request thingy")
}

makeHttpRequest("http://blah")

// you can have an "optional tuple" to express functions that might return a tuple or nil
func makeHttpRequest2(url: String) -> (statusCode: Int, content: String)? {
    // ...
    return (statusCode: 400, content: "bad request thingy")
//    return nil
}

if let (code, text) = makeHttpRequest2("http://blah") {
    print("got \(code) and \(content)")
} else {
    print("bad thing happened")
}


/*:
# Enumerations

* "can" have raw values associated but don't have unless you specify them
* can have "associated values" of any type
    * allows enums to be like
        * discriminated unions
        * tagged unions
        * variants
    in other languages
* each _case_ in the enum can have different associated values
* enumeration cases are NOT assigned a default raw value when created (unlike C)
* are commonly inspected with switch statements
* switch statement MUST be exhaustive when checking for enumeration cases
* raw values
    * are NOT the same as associated values
    * associated data is supplied by you when initializing the instance
    * raw values are compiled into the type itself
* work well for modelling data where there are a fixed number of possibilities to be considered
* enumerations can be recursive
* a recursive enumeration is one that has another instance of the enumeration as an _associated value_ for one of the cases
* you have to use the `indirect` keyword to tell the compiler when you are using recursive enumerations
* recursive enumerations are good for modelling recursive data structures
* they seem similar to typeclasses in Haskell
*/

// create a new Direction type which can only have one of 4 values
enum Direction {
    case North, South, East, West // can have multiple enumeration cases per line
}

let nextStepDirection = Direction.North

switch nextStepDirection {
case .North: // note we don't need the `Direction.` in here to reference the enum case
    print("going north")
default:
    print("going somewhere")
}

// each case in Direction2 has different associated value (that value is a tuple in some cases)
enum Direction2 {
    case North(Int, Int)
    case South(Float)
    case East(String)
    case West(String, Int, Float)
}

let myDir = Direction2.North(33, 44)

// using a switch statement to extract the associated values of an enumeration
switch myDir {
    // case .North(let a, let b): // also valid
    // case let .North(a, b): // also valid
case var .North(a, b):
    print("\(a) and \(b)")
default:
    print("go somewhere else")
}

// an enumeration with raw values
// * note that the type of the raw value must be the same for all cases AND must be declared in the enumberation

enum Direction3: Character {
    case North = "N"
    case South = "S"
    case East = "E"
    case West = "W"
}

Direction3.North.rawValue
Direction3.South.rawValue
Direction3.East.rawValue
Direction3.West.rawValue

enum Direction4: Int {
    case North = 12
    case South // 13
    case East  // 14
    case West  // 15
}

Direction4.North.rawValue
Direction4.South.rawValue
Direction4.East.rawValue
Direction4.West.rawValue

// enums with raw values automtically get a "failable raw value initializer"
// * it has to be failable (return an optional) because you could pass a raw value that
//   will not match a known case
let inputDirFromUser: Character = "S"
let myMaybeDir = Direction3(rawValue: inputDirFromUser)
myMaybeDir.dynamicType
myMaybeDir?.rawValue

enum ArithmeticExpression {
    case Number(Int)
    indirect case Addition(ArithmeticExpression, ArithmeticExpression)
    indirect case Multiplication(ArithmeticExpression, ArithmeticExpression)
}

func evaluate(exp: ArithmeticExpression) -> Int {
    switch exp {
    case let .Number(a):
        return a
    case let .Addition(left, right):
        return evaluate(left) + evaluate(right)
    case let .Multiplication(left, right):
        return evaluate(left) * evaluate(right)
    }
}

// (5 + 4) * 2
let four = ArithmeticExpression.Number(4)
let five = ArithmeticExpression.Number(5)
let two = ArithmeticExpression.Number(2)
let sum = ArithmeticExpression.Addition(four, five)
let product = ArithmeticExpression.Multiplication(sum, two)

evaluate(product)


/*:
# Aside: as! forced conversion

* as! is the "forced conversion operator"
* it is unsafe and may cause the program to crash at runtime

In swift

  ! means "it might trap"
  ? means "it might be nil"
*/

// let fullLine = NSString(data: rawLine, encoding:NSUTF8StringEncoding) as! String


/*:
# Properties

Two types

1. Stored properties
    * available on
        * structs
        * class
    * not available on enumerations
2. Computed properties
    * available on
        * structs
        * class
        * enumerations

Can be stored on

1. Instance properties
2. Type properties (aka class variables in ruby)

Variable properties can be marked as lazy (constants cannot)
* a lazy property is not actually initialized until its value is asked for
* lazy properties do not play nicely with multiple threads accessing the struct/class at the same time
    * there is no garuantee that the lazy property would only be initialized once

Computed properties

* must be declared with `var` because their value is not known at compile time
    * this applies even if it is a read-only computed property

Property observers

* cannot be added to lazy stored properties
* can be set on stored and computed properties
    * it doesn't make sense to use them on computed properties you define in current class but you can
      *add* observers to properties from a superclass without caring whether they are stored or computed
* they are away of adding triggers to properties you don't control but you wish were computed!
* property observers are called evne when the new property value is the same as the old value


Type properties

* are always lazy (no keyword required)
* must always have a default value assigned (the type itself has no initializer to run)
* introduced by the `static` keyword for classes, structs, enumerations
* for computed properties only within classes you can use the `class` keyword
    * `class` variables & constants can be overriden by child classes (unlike static variables & constants)
*/

class PropsThing {
    let foo1: String // stored property, constant, on instance
    let foo2: String = "default value" // instance stored property w. default value

    var foo3: String // stored property, variable, on instance
    var foo4: String = "default value" // stored property, on instance, w. default value

    // lazy variable stored property, someExpensiveOperation is not run during initialization
    lazy var yawn: String = self.someExpensiveOperation()
    // Question: self is required above. why? perhaps because we are not within a method???

    static var someSimpleTypeAkaClassVariable: String = "blah" // NB: static => lazy

    // This doesn't work because it is a stored property not a computed property
    // class var someSimpleTypeVariableThatCanBeOverriddenBySubClass: String = "blah"

    // This computed type property can have its implementation overriden by subclasses
    class var someSimpleTypeVariableThatCanBeOverriddenBySubClass: String {
        return "blah"
    }

    var someSimpleThing = "simple"

    var someComputedProp: String {
        get {
            print("getting computed thing")
            return "result"
        }
        set(newThing) {
            someSimpleThing = newThing
        }
        // if you don't supply explicit paramater name to set it defaults to 'newValue'
        // set { 
        //   self.foo1 = newValue 
        // }
    }

    // you can use short-hand syntax if you don't need a setter
    var readOnlyCompProp: String {
        return "foo"
    }


    var propWithObservers: String = "FOO" {
        willSet { // newValue is implicit arg
            print("The property \(propWithObservers) is about to be changed to \(newValue)")

        }
        didSet { // oldValue is implicit arg
            print("The value was just changed from \(oldValue) to \(propWithObservers)")
        }
    }

    // initializer must exist and put values in all stored properties that don't have default values
    init() {
        foo1 = "foo 1"
        foo3 = "foo 3"
    }

    func someExpensiveOperation() -> String {
        return "i was hard work"
    }
}

// querying type properties
struct SomeYoke {
    static let x = "yy"
    let instance_x = "xx"
    var computedThing: String {
        return "val"
    }

    var simpleValue = "foo"

    var longHandComputedProperty: String {
        get {
            return simpleValue
        }
        set {
            simpleValue = newValue
        }
    }
}

SomeYoke.x // query the property on the type not an instance of the type
let yoke = SomeYoke()
yoke.instance_x
//yoke.x



/*:
## A story about value and reference types

*/

// If we create a struct with variable members ...
struct Thing {
    var a: Int
    var b: Int
}

// ... and then create an instance of it that we assign to a constant ...
let immutableThing = Thing(a: 2, b: 5)

// ... then the members of the struct are constant even though they were introduced with "var".
// immutableThing.a = 6 // compiler error,

// If you do the same thing with a class ...
class ClassThing {
    var a: Int
    var b: Int

    init(_ local_a: Int, _ local_b: Int) {
        a = 44
        b = 66
    }
}
let apparentlyImmutableClassThing = ClassThing(2, 5)

// ... then you can assign a new value just fine
apparentlyImmutableClassThing.a = 44


/*:
This is because structs are value types and classes are reference types.

To summarise

* When an instance of a value type is marked as constant all its properties are also marked constant
* When an inteance of a reference type is marked constant it does not affect the mutability of its properties

*/

// Global and local variable can be computed properties and have observers too
var secret = "blah"
var someGlobal: String {
    get {
        return secret
    }
    set {
        secret = newValue
    }

    // while you can set observers on global properties you cannot set them on a global computed property
    // willSet {
    //    print("\(someGlobal) is about to become \(newValue)")
    // }
    // didSet {
    //    print("\(oldValue) become \(someGlobal)")
    // }
}

someGlobal
someGlobal = "boo"
secret

func justProvidesAScope() {
    var secret = "blah"
    // local function variables can be computed too
    var foo: String {
        get {
            return secret
        }
        set {
            secret = newValue
        }
    }
}

// You can use observers to make a stored property from a superclass behave like a computed property in your class
//class Parenty {
//    var someVal = "foo"
//}
//class Childy: Parenty {
//    var someVal: String {
//        willSet {
//           print("\(someVal) is about to become \(newValue)")
//        }
//        didSet {
//           print("\(oldValue) become \(someVal)")
//        }
//    }
//
//    init() {
//        super()
//    }
//}
//TODO: make the above work when I know about inheritance


/*:
# Methods
 
* classes, structs, enumerations: can hae instance methods
* classes, structs: can hae class aka type methods


self

* within a method `self` refers to the instance of class|struct|enum 
* you don't need `self` to access your own instance methods or properties so you rarely need to type it
    * you can use it if the parameter name for a method shadows an instance property because they have the same name
* self is required if you invoke an instance method within a class|struct|enum but outside of a method e.g. a dynamic initialization


The most general form of a swift method signature is

func methodName(externalName1 internalName1: Type1, externalName2 internalName2: Type2, ...) -> ReturnValue {
}

If you only pass one name for each parameter swift does

* For the first parameter
    * assume that the name you gave is an internal name and the external name is _ (ignored)
* For all other parameters
    * assume the name you gave is both the internal and external parameter


This allows you to use the first parameter without a label but all others require one e.g.

methodName(val1, externalName2: val2, ...)


When swift method signatures are written in documentation they only show the 
external names e.g.

    incrementBy(_:numberTimes:)

This tells you how to invoke the method - it tells you that the first parameter 
should *not* have a label but that the others should

*/

class Counter {
    var counter: Int = 0
    func incrementBy(amount: Int, numberTimes: Int) {
        // 1st param: external name = _, internal name = amount
        // 2nd param: external name = numberTimes, internal name = numberTimes
        counter += amount * numberTimes
    }
}

let cc = Counter()
cc.incrementBy(1, numberTimes: 4)
cc.counter
cc.incrementBy(2, numberTimes: 5)
cc.counter

/*
# Value type properties are immutable by default

* By default the properties of a value type (struct|enum) cannot be modified by its methods
* If you mark method with `mutating` keyword it can mutate properties

Structs and enums are "value type"

* variable struct properties can be modifed from outside
* variable struct properties cannot be modifed from inside by methods unless method marked `mutating`
    * structs try to make immutability the happy path - they force you to explicitly mark mutating methods as so. But isn't it a bit inconsistent to allow mutation from outside?
* constant struct properties cannot be modifed from outside
* constant struct properties cannot be modifed from inside

a "mutating" method can replace `self` with an entirely new instance of the Struct. in thke case of an enum it would replace self with a new case from the same enumeration


Type methods

* work very similar to class methods in ruby:
    * `self` refers to the type (again `self` is not all that commonly used - mostly used to tell the difference between parameter names and type property names
    * it can call other type methods of the same type without needing to prefix the name with the type
*/

struct CanIBeMutant {
    var x = "default x value"
    var y = "default y value"
    let consty = "I am default consty"

    // func changeX() { x = "blah" } // compiler error
    mutating func changeX() { x = "new x value" }
}

var professorX = CanIBeMutant()
professorX.changeX()
professorX.x
professorX.y = "hello new y"
professorX.y
professorX.consty


func doThings(var a: CanIBeMutant) {
    a.changeX()
    print("\(a.x)")
}

var m1 = CanIBeMutant()
doThings(m1)
m1.x


/*:
# Subscripts

* class|struct|enum can define subscripts
* swifts way of letting you define methods that are invoked with foo[param1] not foo.dothing(param1)
* swift uses them for arrays and dictionaries
* subscripts let you access data from an instance based on some key you provide
    * e.g. a numerical index for an array or a key value for a dictionary
* subscripts 
    * can be multiple dimensions
    * can be read/write (getter + setter) or read-only (getter only)
* can 
    * use variadic parameters
    * use variable paraemters
* cannot
    * inout parameters
    * provide default parameter values
* a class or structure can provide as many subscipt implementations as it needs and the correct one will be found by type

There is more detail on subscripts in the book but it seems like a pretty niche feature so
I'm going to leave it for when I actually need it.

*/

struct EoinCrazyDict {
    // eoinDict[44]
    subscript(index: Int) -> Int {
        get {
            return 33
        }

        set {

        }
    }

    // eoinDict["hello"]
    subscript(index: String) -> Int {
        get {
            return 33
        }

        set {

        }
    }
}

/*:
# Inheritance

* classes can inherit, structs and enums cannot
* swift does not have a base class e.g. BasicObject that all classes inherit from
* uses "subclass" and "superclass" not parent/child as terminology
* use `override` keyword to indicate when you are overriding a superclass thing (property, method, subscript etc.)
* overrides 
    * must have the same type signatures
    * must have a corresponding definition in the superclass chain
* `super` keyword
    * use it to access 
        * methods: super.doThing()
        * properties: super.thing
        * subscripts: super[33]
      from higher up on the superclass chain
* you cannot add property observers to inherited constant or read-only properties
    * it wouldn't make sense to observe changes in them because they never change

* `final` keyword
    * used to prevent overrides
* instance methods, instance properties, type methods, type properties, subscripts etc. can all be marked as final
* whole classes can be marked as final too - this makes any attempt to inherit from the class a compile time error
*/

class Vehicle {
    final var speed = 0.0 // subclasses cannot override speed!
    var description: String {
        let msg = "Travelling at \(speed)"
        // print("\(msg)")
        return msg
    }
}

var thingy = Vehicle()
thingy.speed
thingy.description

class Bicycle: Vehicle {
    var hasBasket = false

    override var description: String {
        return "Biking at \(speed)"
    }
}

var bike = Bicycle()
bike.speed = 41
bike.description

class Tandem: Bicycle {
    var colour = "White"
    override var description: String {
        return "Tandem \(super.description)"
    }

    var longDesc: String {
        return "long: \(super.description)"
    }
}


var tandy = Tandem()
tandy.speed = 55
tandy.description
tandy.longDesc


/*:
# Initialization

TODO:
*/

/*:
# Deinitialization

TODO:
*/

/*:
# ARC

TODO:
*/


/*:
# Optional chaining

* the `?.` operator
* a property that normally returns an Int will return an Int? when accessed through optional chaining

    <an optional that happens to be nil>?.<a property or method call that normally returns type T>

evaluates to

    <optional T>

* things that would normally return `Void` e.g. assignment or calling a method that doesn't have an explicit return
  will return `Void?` when called with optional chaining
    * this means you can test them against `nil` to see if they worked or not

if josh.residence?.address = Address() != nil {
    // all is well
}

if josh.residence?.doThingThatReturnsVoid() != nil {
    // all is well
}

* you can use optional chaining with subscripts e.g. thing?[0]
*/

class Person {
    var residence: Residence?
}

class Residence {
    var numberOfRooms = 1
}

var joe = Person()
joe.residence

// This will fail *at runtime* if residence is nil
// joe.residence.numberOfRooms

// this will return nil if residence is nil so will not fail at runtime
// joe.residence?.numberOfRooms

if joe.residence?.numberOfRooms != nil {
    print("Joe has rooms")
} else {
    print("joe has no home")
}

// . returns the value of the 'son' property on optionalMan

// ?. always returns an optional


/*: 
# Runtime errors

* swift calls runtime errors "traps"

*/


/*:
# Initialization

* swift requires that all properties have values after initialization
    * stored properties CANNOT be left in an indeterminate state
    * this includes any stored properties it got through inheritance
* you can provide values either by
    1. providing a default value
    2. assigning a value in an initializer
* swift initializers do not return a value
* swift provides an automatic external parameter name for EVERY parameter in an initailzier
    * unlike in regular functions where the first parameter does not get an extenal name by default
    * if you want to call initializers parameters without names e.g. Foo(34) you must explicitly use `_`
* within an initializer you can call `self.init` to call another intializer of the same type
    * `self.init` is ONLY available within initializers
* enums and structs have just one kind of initializer
* classes have two kinds:
    1. designated initializer
        * fully initializes all properties introduced by the class
        * calls _an_ appropriate IMMEDIATE superclass DESIGNATED initializer to continue the initialization process up the superclass chain
        * designated initializers are few in number and form "funnel" points through which initialization continues up the superclass chain
        * designated initializer MUST call a designated initializer from its IMMEDIATE superclass
            * => a designated initializer cannot call a convenience initializer from the superclass
            * => a designated initializer cannot call a designated initializer from a class higher than its superclass
        * every class MUST have at least one designated initializer
            * in some cases it is enough if a class inherits a designated initializer from a superclass
        * there can be multiple designated initializers
    2. convenience initializer
        * secondary, supporting initializers
        * they provide convenicne WRAPPERS around designated intializers i.e. they must call a designated initializer from the same class - they cannot call outside the class!
        * if you use `self.init` in an initializer, swift expects you to mark it as "convenience"
        * => "convenience" tag implies that this thing only calls initializers from the current class
        * there can be multiple convenience initializers
* summary of differences between designated and convenience
    * designated initializers always delegate _up_
    * convenicnce initializers always delegate _across_
* swift runs initializers in two phases
    1. working its way up the chain it invokes initializers to set default values for the properties they introduce
    2. wor king its way down the chain it runs initializers in reverse order and allows them to customise the properties of the (now valid and property initialized) object
* This implies that 
    * convenience initializers should 
        1. call `self.init`
        2. do whatever customisation they need to do
    * designated initializers should
        1. set default values for any properties they introduce
        2.call `super.init(...)` (if they need to) 
        3. do whatever customisation they need to do
* initializer inheritance
    * in general swift classes do NOT inherit any initializers from their superclass
* but there are exceptions:
1. provide default values for all the properties you introduce in the subclass
2. 
    * case 1: if subclass doesn't define ANY designated initializers it inherits ALL the _designated_ initializers from the superclass
    * if subclass implements ALL designated initializers from superclass it automatically inherits all the convenience initializers from the superclass
The general gist is that a subclass will inherit all superclass initializers that will still initialize the object correctly
* consequences
    * if you have some data in a class|struct|enum that you cannot set a value for during the first phase of initialization then you must set it to be an optional type so that it can be initialized to `nil` by swift

Failable initializers

* sometimes you need to create an object but are not certain it will succeed
* build an optional of the class 
* uses `return nil` syntax (initializers don't really return a value, the `return` keyword is overloaded here)
init? { ... }
*/


// swift provides an "default initializer" if you have provided a default value for all properties
struct StructWithAutomaticIntializer {
    var a = 3
    let b = "hello"
}
StructWithAutomaticIntializer().a

class ClassWithAutomaticIntializer {
    var a = 3
    var b = "hello"
}
ClassWithAutomaticIntializer().a

enum EnumWithAutomaticIntializer: Int {
    case a = 3
    case b = 4
}
EnumWithAutomaticIntializer.a.rawValue

// structs also get an automatic "memberwise initializer" which has a named parameter 
// for each _variable_ property (constant properties are rightfully ignored)
StructWithAutomaticIntializer(a: 44)



class Color1 {
    var red, green, blue: Double

    // Example of using _ in an initializer to all its use without parameter names
    init(_ red: Double, _ green: Double, _ blue: Double) {
        self.red = red
        self.green = green
        self.blue = blue
    }

    // a second designated initializer
    init(white: Double) {
        // option 1: set values as usual
        self.red = white
        self.green = white
        self.blue = white
    }

    // a convenience initializer
    // * invokes `self.init` so compiler forces us to mark it as "convenience"
    convenience init(all: Double) {
        self.init(all, all, all)
    }
}

Color1(22,33,44)
Color1(white: 22) // notice that the external name is enforced for the first arg


/*
* enums
    * failable initializers are handy in enumerations that get initialized with some raw value (where only some of the possible values of the raw value have a matching case)
    * enumberations with raw values automatically get a failable initializer
* classes
    * a failable enumeration can only fail AFTER it has set default values for all introduced properties AND call any superclass initializer it needs to i.e. you have to pass all the memory setup safety checks before you can fail with a nil

* failable initializers and delegation
    * they can delegate to another initializer (failable or not) in the subclass
    * a subclass failable initializer can delegate to a superclass failable initializer
* you can override a superclass failable initializer with a subclass non failable initializer
    * you cannot do it the other way round i.e. override a superclass non-failable initializer with a failable one

You can also create an implicitly unwrapped initializer via

    init!{ ... }

* prefix initializer definition with `required` to indicate that all subclasses must provide it


As an alternative to putting all you initialization stuff in init() functions you can attach a closure to a property that is run during initialization and its return value is used as default value for the property
*/

class Animal {
    let species: String

    init?(species: String) {
        self.species = species
        if species.isEmpty {
            return nil
        }
    }
}

let Lion = Animal(species: "Cat")
let Cougar = Animal(species: "")


enum TemperatureUnit {
    case Kelvin, Farenheit, Celsius

    init?(unit: Character) {
        switch unit {
        case "K":
            self = .Kelvin
        case "F":
            self = .Farenheit
        case "C":
            self = .Celsius
        default:
            return nil
        }
    }
}

TemperatureUnit(unit: "F")
TemperatureUnit(unit: "X")


// enums with raw values automatically get a failable initializer
enum TemperatureUnit2: Character {
    case Kelvin = "K"
    case Celsius = "C"
    case Farenheit = "F"
}

TemperatureUnit2(rawValue: "F")
TemperatureUnit2(rawValue: "x")

// using a closure to calculate a "smart" default value
class Foo {
    let title: String = {
        return "hello"
    }() // <---
}

Foo().title