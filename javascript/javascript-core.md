Javascript : My Definitive Guide
================================

General points about JS
-----------------------

* Javascript is written using the Unicode character set (a superset of ASCII & Latin-1)
* Javascript is case sensitive
* JS is dynamically typed i.e. you never specify a variable's type. JS automatically converts variables from one primitive type to another based on the context.
* JS does "mark and sweep" garbage collection.
* single and double quotes are equivalent in JS - special characters are always interpolated.
* Javascript is functional like lisp and scheme

Key ideas in JS
---------------

*	Load & go delivery: programs are delivered as source code
*	Loose typing:
		? what are benefits of loose typing?
*	Objects are general containers
		The unificaiton of objects and hash tables
*	Prototypal inheritance
*	Lambda = the use of functions as first-class objects
*	Linkage through global variables
		turns out to be a very bad idea


Literal
-------

A literal* is a data value that appears directly in the program e.g.
	12 == a number
	"hello world" == a string literal
	/^hello/gi == a regular expression literal
	null == a primitive value that represents the intentional absence of any object value

*	literals are expressions return their value when evaluated e.g. 45 is evaluated to be the numer 45

Comments
--------
DC recommends not using /* */ comments as reg exps can contain that sequence.

Identifier
----------

The names of variables, called identifiers, conform to certain rules. A
JavaScript identifier must start with a letter, underscore (_), or dollar sign
($); subsequent characters can also be digits (0-9). Because JavaScript is case
sensitive, letters include the characters "A" through "Z" (uppercase) and the
characters "a" through "z" (lowercase).

	var x; // legal
	var $; // legal
	var $45; // legal
	var 45; // Not legal
	var _45; // legal

* By convention variables, parameters, members, function names start with lower case. Except for constructors whiech begin wiht Uppercase to help identify them as they are dangerous to use without 'new'.

Expressions
-----------

*	https://developer.mozilla.org/en/JavaScript/Guide/Expressions_and_Operators
*	A "phrase" of the language
*	A valid unit of code that resolves to a value
*	An expression is also a statement

There are 2 types of expressions;

1.	Those that assign their resolved value to a variable
2.	Those that just resolve to a value



Statements
----------

*	A statement in JS ends in a semi-color and is kind of a "full sentence" if we think of expressions as "phrases"
*	Any expression is also a statement.
*	You can omit the semi-colon at the end of a line if there is only one statement on the line but it's bad practice to do so.
*	A javascript statement usually ends in ; but can interpret newline as the end of a statement.
*	Statements can have labels - useful for breaking out of loops, but only use them where it makes sense (while, do, for) e.g.

statements types in JS:

*	block
*	comment
*	Conditional statements
	*	if/else
	*	switch
*	Looping Statements
	*	for
	*	do while
	*	while
	*	label
	*	break
	*	continue
*	Object Manipulation statements
	* for ... in
	*	for each ... in
*	Exception handling statements
	* try ... catch
	*	throw
* Full list here: https://developer.mozilla.org/en/JavaScript/Reference/Statements

	label: var x = 12; // legal but not useful

	// More useful
	var i;

	outer:
	for (i = 0; i <= 10; i++) {
		for (j = 0; j <= 10; j++) {
			if (foo) {
				// break; // Transfers program control to the statemetn immediately followign the current loop/switch
				break outer; // Transfers program control to the statement following the statement labeled 'outer'
			}
		}
	}

	Statements seem to have a left and right side

Switch Statement
----------------

* JS switch statement has a fallthru hazard - you have to explicitly break after each clause
* DC reckons that the JS switch statement is a sort of "computed goto" statement where you have labels. he thinks this is a weakness.

switch (expression) { // the switch value does  not have to be a number which is handy!
	case 'x': // the case values do not have to be constants - they can be expressions

		break; // each clause has to be explicitly termintated

	case 'y': // this can be the result of an expression
		console.log('got a y')
		break; // each clause has to be explicitly terminated

	default: // catch the default case
		console.log('got nothing');
}

 	var a = 12;

  	switch (a * 2) { // Notice that the switch value can be an expression

  		// As long as you use break statements, Only the first matching case will be executed
  		// If you omit the break statements the program will execute every matching clause including the default
  		case 24:
  		case "hello": // you can associate a block of statemetns with multiple matches like this
  			console.log('got two');
  			// break; // If I omit this, then JS will continue executing the statements in the following clauses until it encounters a break
  		// It is recommended that deliberate fall-throughs be commented as such

  		case 33:
  			console.log('got 33');
  			// *DELIBERATE FALLTHROUGH*
  			// break;

  		default: // Usually the default clause it at the end but this doesn't have to be the case
  			console.log('got nuthin');
  			// break;

  		case 12*2: // Notice that the case value does not have to be constant - it can be an expression
  			console.log('got it');
  			// break;
  	}

With statement
--------------
* Don't use it.
* ES5 strict removes this.

try/catch statements(Throwing Exceptions)
------------------
JS has a number of built-in error types:
* Error
* EvalError
* rangeError
* SyntaxError
* TypeError
* URIError

The language is quite permissive so doesn't throw exceptions very often

try {
	// do stuff
} catch (e) {

}

Function Statement
-----------------

Var Statement
-------------
* defines variables within a function
* if you don't give an initialisation value it will be set to undefined for you

Subtle thing about var:
	var x = 12; //returns undefined;

var y; // returns undefined
y = 12; // returns 12;

This has implications in default value setting

Consider:
	foo = this.bar || (this.bar = {});
The RHS of the || expression will return {} which will in turn be used as the RHS of the assignment

Why does this happen?
	not sure.



Return Statement
----------------
has 2 forms:
	return expression;
	or
	return; // returns undefined except if in a constructor where we return this
JS functions *always* return a value!


continue, break, label
----------------------

*	labels create a target for continue, break statements
*	https://developer.mozilla.org/en/JavaScript/Reference/Statements/break
*	https://developer.mozilla.org/en/JavaScript/Reference/Statements/label
*	https://developer.mozilla.org/en/JavaScript/Reference/Statements/continue
*	Don't use too many labels - makes code hard to understand. Use function calls or throw errors instead
*	labels are used by continue, break only.

6 Values/Types
==============

* JS is loosely typed - it is not untyped.
*	Any variable can receive any of the 6 value types.
* Any kind of type can be used as a parameter to a function or returned from a function
*	JS has 2 categories of 'types' - 5 primitive types and an object type
there are 5 primitive types and *everything* else is an object
* Everything except null and undefined will behave like an object

1. Numbers
2. Strings
3. Booleans
4. null (a language keyword) (the inten)
5. undefined (a predefined global variable) undefined is a primitive value used when a variable has not been assigned a value
6. Objects (any value that is not one of the other 6 types is *an object*)
JS types can be divided into types with methods and types without

Technically only objects have methods in JS but  numbers, booleans and strings
behave as if they have methods too. null and undefined do not have any methods.

* With Methods
	* Numbers
	* Booleans
	* strings
	* Objects
*	Without
	* null
	* undefined

JS types can be divided into mutable (can be changed) and immutable (cannot):

* Mutable:
	* Objects
		* Arrays
* Immutable:
	* numbers
	* booleans
	* null
	* undefined
	* strings (JS provides no way to alter the text of an existing string)

immutable types are compared by value while mutable types (objects) are
compared by refernce (2 objects (incl. arrays etc.) are the same iff they refer
to the same underlying object. This also means that assigning an object to a
variable simply assigns the *reference* - it does **not** create a new copy of
the object.

JS will convert from one type to another as it sees fit.
JS variables are 'untyped' => you can assign any kind of value to a variable (no matter what type it had previously)
JS uses 'lexical scoping' i.e. scope comes from where the variable is declared. Variables declared out side of a function are global and visible everywhere - they have 'global scope'
Variables declared in a function are local to that function - the have 'function scope'
In JS you can refer to a variable declared later! this is called hoisting as the declaration (but no initialization) of the variables are "hoisted" or lifted to the top of the function or statement.


What does "first class" mean?
-----------------------------

A first class object in the language can be
* stored in a variable
* passed as a parameter
* returned from a function
* stored in an object

const keyword (no IE support)
-----------------------------

Use the const keyword to declare a constant variable
	const MY_VAR = 33;
variables declared with const have the same scope as variables declared with var.
WARNING: const is not supported in IE6-9. It will be in ECMAScript 6 but probably with different semantics.

Numbers
-------
* Only one number type in JS
* Internally represented as 64 bit floating point aka 'Double'
* This means it only gives you an approximation of deciamal values

* NaN is a special number (even though name stands for 'Not a Number')
	* result of undefined or erroneous operations
	* NaN is toxic i.e. any arithmetic operation with NaN as an input will have NaN as a result
	* NaN is not equal to anything, including NaN
	* typeof NaN // returns 'number'
	* in JS you don't get an excpetion if  you are doing bad math, you just get NaN

* Number function
	* converts a value into a number
	* produces NaN if it can't convert the string into a number

* parseInt(value, 10)
	* converts the value into a number
	* it stops as the first non-digit character so always give radix argument

	parseInt("08") === 0 // stops at first non-digit
	parseInt("08", 10) === 8
* Math object contains a number of useful functions: abs, floor, log, max, andom, in, sqrt etc.
* Divison of two integers can yield a non-integer result (unlike in other langs that have an Integer type)
	10 / 3 // => 3.3333333333
* The value Infinity represents all values greater than 1.79769313486231570e+308

Associative Law does not always hold

	(a + b) + c === a + (b + c)

* Produces false for some values of a, b, c
* Works ok for Integers under 9 quadrillion

* Decimal fractions are approximate

	a = 0.1;
	b = 0.2;
	c = 0.3;

	(a + b) + c // 0.6000000000000001
	a + (b + c) // 0.6
	(a + b) + c === a + (b + c) // => false

* IF YOU ARE EVER DEALING WITH MONEY EXPRESS EVERYTHING IN CENTS FIRST

Number Methods
*	toExponential
*	toFixed
*	toLocaleString
*	valueOf
* toPrecision
*	toString

33.toString(); // Doesnt work as parser thinks it's a decimal point
(33).toString(); // works
33 .toString(); // works
33..toString(); // works
33['toString'](); // works

All numbers inherit from Number.prototype so you can augment all numbers if you want
Numbers are first-class objects
* can be stored in a variable
* can be passed as a parameter
*	can be returned from a function
* can be stored in an object

undefined, NaN, Infinity are not reserved words but you're a moron if you use them.
Javascript Math = a built in object https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/Math

explicit converstion between types
----------------------------------

Test which of the built in types a variable is = typeof operator
https://developer.mozilla.org/en/JavaScript/Reference/Operators/Special_Operators/typeof_Operator

how to tell whether something is an object or an array:
	typeof is no help - returns "object" for both
	use Array.isArray(thing);


Strings
-------

* sequence of 0 or more 16 bit unicode characters (UCS-2, not quite UTF-16). JS was created before UTF-16
* UCS-2 has no awareness of surrogate pairs
* No separate character type in JS - a char is just a string with one char in it
* Strings are immutable (once made, they cannot be modified)
* all characters in JS are 16 bits wide:
	"A" === "\u0041"
* Similar strings are equal (==)
* String literals can be written with ' or " - both work exactly the same way
* string.length = num chars in the srint

* String function - converts value to a string
	str = num.toString(); // works but throws exception on null, undefined as they don't have a .toString method
	str = String(num); // works for null, undefined

* Convert a string into a number
	num = Number(str);
	num = +str;
	num = parseInt(str, 10); // ALWAYS include the radix when using parseInt

Useful string functions:
escape() unescape()
encodeURI() decodeURI()
encodeURIComponent() decodeURIComponent()

Booleans
--------

There are two boolean values in JS: true, false
* Boolean(value)
	* boolean function

Truthy & Falsy
==============

Truthy: Something which evaluates to TRUE.
Falsey: Something which evaluates to FALSE.

JS has the notion of 'truthy' and 'falsy'. Any time JS expects a boolean value,
a 'falsy' value works out false and a 'truthy' value works out true. There are 7
falsy values in JS:

1. false
2. null
3. undefined
4. "" (empty string)
5. NaN
6. 0
7. -0

All 7 of these will be false when we use !! to force them to a boolean
NaN === false; // => false as NaN is not equal to 0
(!! NaN) === false; // => true

It is possible to explicitly wrap a primitive (string, number, null, undefined,
boolean) in an object, which will make it truthy. For example, 0 (zero) is
falsey, but new Number(0) is truthy. A scarier example is new Boolean(false)
which is also truthy! Be careful. Only very rarely should you need to explicitly
wrap primitives.

The falsy values false, 0 (zero), and "" (empty string) are all equivalent and can be compared against each other:

	console.log(false == false); // true
	console.log("" == false); // true
	console.log(0 == false); // true
	console.log(-0 == false); // true

The falsy values null and undefined are not equivalent to anything except themselves:

	console.log(null == false); // false
	console.log((!! null) == false); // true

Finally, the falsy value NaN is not equivalent to anything — including NaN!

	console.log(NaN == false); // false
	console.log((!! NaN) == false); // true
	NaN === NaN; // false
	// https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/NaN
	// Equality operator (== and ===) cannot be used to test a value against NaN. Use isNaN instead.

All other values in the language are truthy. Watch out for:
*	!! "0" 		// true because it is a non-empty string
*	!! "false" 	// true because it is a non-empty string

You can force to boolean with !!

var myGaruanteedBoolean = !!3; // true
var myOtherBoolean = !! NaN; // false

The JVM runs an internal ToBoolean operation when evaulating comparison
statements so, internally, it does the equivalent of !! on the values you supply
in if/while etc. statements.

The ToBoolean thing that we referenced above happens in all logical contexts in JavaScript. For example, the && (logical AND) operator
http://james.padolsey.com/javascript/truthy-falsey/


null
----

* indicates that the value is intentionally missing
* A value that isn't anything
* null is a language keyword or primitve value? which is it??
* null will evaluate as 0 in a numeric context and as false in a boolean context

var x2 = null;

if (x2) {
	console.log('x2 is true');
} else {
	console.log('x2 is false');
}

console.log(x2 + 2); // => 2 because x2 evaluates as 0 in a numeric context

undefined
---------

* the default value for variables
* the value of missing members in objects
* Strangely, a variable can be defined and have the value undefined - confusing naming
* Variables that are not yet given a value are automatically given the 'undefined' value
* It is a primitive value in javascript that you can test for.

if (a === undefined) {
	// + is the string concatenation operator in javascript
	console.log('a is:' + a);
}

// undefined behaves as false in a boolean context
var x1;
if (x1) {
	console.log('it is true');
} else {
	console.log('it is false');
}

The difference between null and undefined
-----------------------------------------

* null indicates the absence of a value (null is a language keyword)
* undefined indicates a deeper absence. undefined is a predefined global variable (not a language keyword like null)

*	null is aprimitive value that represents the _intentional_ absence of any object value
	undefined is a primitive value used when a variable has not been assigned a value
	null value means that it is deliberately empty, undefined means it is empty but we don't know if it's intentional or not

undefined is a global singleton object
* you can pass it as a function arg e.g.
	doStuff(undefined, 23, "Eoin");

When you access a property of an object which doesn't exist, you don't immediately get an error like in every other language. Instead you get an undefined object.

Testing whether a property exists in an object

if (o.prop !== undefined) {
	// doesn't work as o.prop might exist and have the value undefined e.g.
	// var o = {};
	// o.prop = undefined;
}

if ('prop' in o) {
	// works
}





*****************************

Objects
=======

*	Everything in JS that is not one of the 5 simple types is an object.
* An object is an unordered, dynamic collection of *properties*
* Even though objects are hash tables, there is no hash nature visible.
* In JS an object is a dynamic collection of properties. This makes is very different to othe languages where an object is an instance of a class (which specifies some state and behavior)
* Each property has a keystring which is unique within that object


* Object = an unordered, dynamic collection of properties. Has some attributes itself
* Property = a named collection of attributes

* A property name is anything that evaluates to a valid JS string

* The fundemental operations for objects are get,set,delete

* get operation:
	object.name
	object[expression]
		Tip: If you need to use an expression that evaluates to a property name use []
		Tip: object["eoin kelly"] is valid but you have to use [] notation to get at it

*	set operation (similar to get except it is in the LHS of the statement)
	object.name = value;
	object[expression] = value; // Again, expression can be any JS expression that returns the name of the property

*	delete operation
	delete object.name;
	delete object[expression];


Finding out what is in an object (3 ways)
-----------------------------------------

1. for...in
		* traverses all the enumerable properties of an object and its prototype chain

2. Object.keys
		* returns an array of all the own property names in the object (doesn't go up inheritance chain)
		* only returns properties which have enumerable set to true

3. Object.getOwnPropertyNames
		* returns an array of all the own property names in the object (doesn't go up inheritance chain)
		* ignores the enumerable flag



Function objects

Object object
-------------

* Object is a special kind of function object created by the system. (special in that it doesn't inherit from Function.prototype unlike all other functions in the system)
* It has a property called 'prototype' which itself contains an object.
* The prototype object has the following 11 properties (in chrome at least):
	1 constructor
	2 hasOwnProperty
	3 isPropertyOf
	4 propertyIsEnumerable
	5 toLocaleString
	6 toString
	7 valueOf
	8 __defineGetter__
	9 __lookupGetter__
	10 __defineSetter__
	11 __lookupSetter__

* The prototype object is the thing that all other objects in the system inherit from - note that they don't inherit from Object itself.
* By default, none of the 13 properties are enumerable (so they won't show up in your for...in loops). Obviously anyone can add stuff to Object.prototype so you can't rely on this
* If the property is not enumerable it's name will appear as a faded purple in chrome dev tools

* This Object.prototype is unique in the system in that it doesn't have a __proto__ property pointing to another object. Usually when you create an object it gets a hidden property (called __proto__ in some implementations) that points at another object in the system.

Object.prototype.__proto__ // => null
Object.prototype.constructor ---> points to Object

Only Function objects get a .prototype property by default - remember that Object is actually a constructor function of sorts
The new keyword
Consider .prototype to be the pre-loaded bag of stuff you want to make available to any object created with this function
If  you decide to use this function as a constructor, then .prototype is a pre-defined place that you can put stuff that you want shared between all the objects

I got confused because even tho *all* objects in the system have another object they consider their "prototype", this is separate from the pre-rolled mechanisem JS has for setting that up.

You can decide what an object's __proto__ should point at when you are creating it
Object.create(object you want __proto__ to point at)

Function object
---------------

* In JS function objects contain  both the "constructor fucntion" that sets up the new object *and* an object to use as the prototype of new objects.
* A JS "class" is just a Funciton object that serves as a constructor plus an attached prototype object
* All functions in the system inherit from this special function object
typeof Function // "function"

Function.prototype object contains
	__proto__		---> Object.prototype
	apply
	arguments
	bind
	call
	caller
	length
	name
	wtbind


When I create a function
var f = function (x) { console.log(x); }

f contains properties:

f.prototype
	constructor
			arguments
			caller
			length
			name
			prototype ---> f.prototype
			__proto__ ---> Function.prototype
	__proto__ 				---> Object.prototype


typeof f.prototype.constructor // "function"

f.prototype.constructor points to f?
f.prototype.constructor.prototype === f.prototype // => true
f.prototype.constructor.prototype.constructor === f.prototype.constructor // => true
Is this not a circular reference???

Meta Object API (ES5 Only)
--------------------------

ES5 gives us control over the attributes of the properties of the objects

There are 2 kinds of properties
	1. Data properties
	2. Accessor properties

Each property is a named collection of attributes:

	 | Attribute Name 	| Attribute value
---|------------------|---------------------------------------------------------
1. | value						| any JS value
2. | writable 				| a boolean flag
3. | enumerable				| a boolean flag (says whether property can te enumerated by for..in)
4. | configurable			| a	boolean flag (says whether property can be deleted or other attributes changed)
5. | get							| function (ES5 only), will be called on any attempt to get from the object
6. | set							| function (ES5 only), will be called on any attempt to set to the object

* This collection of attributes is called the "property descriptor"
* 1-4 have always existed but not been available to the programmer before ES5


?? not sure about this
* Data properties can have attributes 1-4
* Accessor properties can have 5-6 (not sure about this)


* so called "public" methods are very flexible - the do all their actions to whatever we pass as 'this' which means we can re-use them easily - e.g. if we have a public method in a parent object, it will happily work with child objects too - it doesn't care what object it was defined on - it only cares about what you give it as 'this'

Working with property attributes (ES5 only)
-------------------------------------------

We have some new methods on Object that let us work underneath the hood:
	// Note that all of these are methods on Object itself

	Object.defineProperty(object, key, descriptor)
	Object.defineProperties(object, object_of_descriptors)

	Object.getOwnPropertyDescriptor(object,key)
	Object.getOwnProperties(object) // Similar to Object.keys() but also returns unenumerable properties

	Object.keys(object)

	Object.getPrototypeOf

Below are two ways of creatign an object literal (both have identical results)

	// Short Version of creating an object literal
	var my_object = { foo: 12};

	// Long Version of creating the same object literal
	var my_object = Object.defineProperties(
		Object.create(Object.prototype), {
		foo: {
			value: 12,
			writeable: true,
			enumerable: true,
			configurable: true
		}
	});

var my_object = { mm: 0 };
Object.defineProperty( my_object,
	'inch',
	{
		get: function () {
			return this.mm / 25.4;
		},
		set: function (value) {
			this.mm = value * 25.4;
		},
		enumerable: true
	}
);

// This is what you see in chrome dev tools when you do the above
Object
	get inch: function () {
	mm: 50.8
	set inch: function (value) {
	__proto__: Object

var fff = {a: 1, b: 2}
fff.keys // => undefined
Object.keys(fff); // => ['a', 'b']
WHY??

Pseudoclassical Inheritance
---------------------------

Netscape gave JS pseudoclassical inheritance (to make Java programmers happier) using 3 bits of gear:
1. Constructor Functions
2. new operator
3. prototype member of function objects

Q: If I don't do pseudoclassical inheritnace can I ignore the prototype member of functions ???
A:

The new operator returns an object with __proto__ set to .prototype member of the constructor objecct - this way we can control what object the new object inherits from. However, using this style __proto__ can *only* point to the .prototype member of a function (unlike Object.create where we can point __proto__ at *any* object).

var x = new Cons(); // x.__proto__ ---> Cons.prototype
// we can't change x.__proto___ at all but we *can* replace/change the Cons.prototype object with another one
?? maybe it's easier to understand if prototype objects are in a consistent place???

x.prototype.constructor // ---> Cons function.
This was intended to allow you to find out from an object what it's constructor was but it doesn't work in practice

* If we replace a constructors original prototype object with an instance of an object of another class then we can inherit that other class's stuff e.g.
	Cons.prototype = new OtherThing();

new keyword = make a new object and call the supplied function giving it the new object as 'this'. Also set the .constructor funciton to point to the constructor function

// Setup our constructor function
function Circle(radius) {
	this.radius = radius;
	this.area = funciton () {
		return this.radius * this.radius * Math.PI;
	};
}

// Create a new empty object and call the constructor on it manually
// remember that === check to see if two variables are referncing the exact same object (not 2 object with the same roperties)

var instance = {};
Circle.call(instance, 5);
instance instanceof Circle; // => false
instance.constructor === Object; // => true
instance.__proto__ === Object.prototype // => true

// Create a new instance the conventional way using new
var instance2 = new Circle(5);
instance2 instanceof Circle // => true
instance2.constructor === Circle // => true
instance2.constructor === Circle.prototype.constructor
instance2.__proto__ === Circle.prototype // => true

Notice that the new keyword also sets the __proto__ link to point to the 'prototype' member of the Constructor function object

Object.getPrototypeOf() is the new, standardized "getter" for the __proto__ link - see http://ejohn.org/blog/objectgetprototypeof/
The .prototype is a property of *function objects only* in JS. You can think of .prototype as a template and __proto__ as a runtime reference to the "parent"
* You can read __proto__ on all browsers but cannot write to it on IE

Prototypal Inheritance
----------------------

1. Make an object any way you like
2. Create new instances with Object.create that inherit from that object
3. Customise the new object

* Taxonomy and classification are not necessary - DC reckons this makes prototypal model much easier
* THis model is sometimes called "differential inheritance" as inherited objects only contain the way they differ from the original ones
	This allows objects to be small

// Polyfill for lack of Object.create in old browsers
if (typeof Object.create !== 'function') {
	Object.create = function (o) {
		function F() {}
		F.prototype = oldObject;
		return new F();
	}
}

if (typeof Object.create !== 'function') {
	Object.create = function (oldObject) {
		var FunctionObject = function () {}
		FunctionObject.prototype = oldObject;
		return new FunctionObject();
	}
}


Objects also have attributes themselves

1. prototype = reference to an object or null
	Object.getPrototypeOf(object) // DC recommends not to use this function
2. extensible = boolean
	Object.isExtensible(object)

	// Locking an object
	Object.preventExtensions(object) // turns off the abilityt add new properties to the object
	Object.seal(object) // .. and turns off configurable bits
	Object.freeze(object) // .. and truns off all writable bits (makes it immutable). btw this is a shallow freeze


Object.create(object, properties ...)
Object.create(null) // Creates an object that does not inherit *anything*
Object.keys(object) // .keys will not give you inherited properties

// Expressing the new opeator in terms of Object.create
function new(func, arguments) {
	var that = Object.create(func.prototype),
			result = func.apply(that, arguments);
	// take the result and if it is an object then return it, otherwise return the object you created
	return (typeof result === 'object' && result) || that;
}
* DC never used the new operator now - he reckons you can do everythign with Object.create
* Also if you use a constructor and forget the new, you will clobber global variables


in ES3 Every property can have zero or more attributes from the following set:
* ReadOnly
* DontEnum
* DontDelete
* Internal

I guess the frist 3 became writeable, enumerable, configurable in ES5 but what about Internal FIXME ???
c
Example:
Object
	[[prototype]] // reference to another object or null
	extensible // boolean attribute of the object itself
	property =	key_string ->	value
								writeable
								enumerable
								configurable
								get (ES5 only)
								set (ES5 only)

	property
			...
	property
			...
	.
	.
	.



In the simple view, we just consider the key_string and value attribute.
* key_string (the name) must be a string
* value can be any type including other objects
* a value can be any value except undefined FIXME huh?
* members can be accessed with . notation or subscript notation:
	myObject.itsproperty
		or
	myObject["itsproperty"]

Don't use new operator to create objects, use Object.create
there is a special 'global object' defined.
normally JS objects are an unordered collection of properties (name-value pairs) but JS defines a special object called Array that stores them ordered.

Object literal Syntax
----------------

* wrapped in { }
	* names can be names or strings
	* values can be expressions
	* : separates name and value
	* , separates the pairs
	* can access the properties with subscript or dot notation
	* myObj.name or myObj['name']
	* cannot use reserved words as names

We can use an object literal anywhere JS expects a value
New members can be added to any object by simple assignment - there are no classes!

var x = Object.create(Object.prototype); // Same as var x = {};
* Notice that if we use object literal syntax we are forced to use Object.prototype as the prototype.
* In some cases the simplicity of creating the object with literal syntax outweights your wish to have reusable stuff in a custom prototype object.

// Object.create(the thing you want __proto__ to point at);

// Create our object
var myobj = {
	name: 'Eoin',
	age: 33
}

// Augment the object
myobj.weight = 'heaps'; // using . notation
myobj[height] = '6ft'; // using [] notation

There are 4 sources of objects in JS:

1. Built into JS: Math, Regexp, Date, Array, String, Boolean, Number
2. Browser Object Model objects: Window, ???
3. Document Object Model Objects: Document, ???
4. Custom objects from delvelopers: YAHOO, etc.



Linkage (Prototypal Inheritance)
--------------------------------

Objects can be created with a secret link to another object.
Mozilla calls this secret link __proto__
If an attempt to access a name fails, the secret linked object is checked for the name.
The secret link is not used when storing a value. New members are only added to the primary object
The object(o) function makes a new empty object with a link to object o.

An object has an internal link to another object (or null) called its prototype.

Note there can be confusion in naming. All objects have an internal link to another object in the system that they consider their "prototype". This can be any object. It is often the case (but not always) that this object is accessed through a .prototype property of yet another object.

The prototype chain is terminated when an object has it's prototype set to null
	Proto1 --> ProtoP1 --> ProtoP2 -- ProtoP3 --> null

var f = function () { return 1; }
// f.__proto__ ---> Function.prototype
// Function.prototype.__proto__ ---> Object.prototype
// Object.prototype.__proto__ = null

* It is a bit confusing above that the 2 objects that f inherits from are accessed as properties of other objects (Function, Object respectively)

// So f inherits all the properties in Function.prototype and Object.prototype
// f.prototype = an object that will be used by system if f used as a constructor
// f.prototype.constructor ---> f
// f.prototype.__proto__ ---> Object.prototype

All objects in JS have an internal link to another object - link denoted by [[prototype]]

All objects in the system inherit from another object *except* for Object.prototype object - it doesn't inherit from anything
another way: every object in JS has a parent.
In ES5 we can use Object.create(null); to create an object that has no __proto__ link


Objects inherit from Object.prototype
Arrays inherit from Array.prototype
Functions inherit from Function.prototype
	Function objects have a .prototype property - others do not


All these *.prototype are objects e.g. typeof Array.prototype // => "object"

function f() {
	return 12;
}
f.prototype // returns the object
// f --> Function.prototype --> Object.prototype --> null

var a = {a: 1};
a. prototype // => undefined (ordinary objects do not have a prototype property 	)
// a ---> Object.prototype ---> null

var b = Object.create(a);
// b ---> a ---> Object.prototype ---> null
console.log(b.a); // 1 (inherited)

var c = Object.create(b);
// c ---> b ---> a ---> Object.prototype ---> null

var d = Object.create(null);
// d ---> null
console.log(d.hasOwnProperty); // undefined, because d doesn't inherit from Object.prototype


function Parent(name) {
	this.name = name; // assigns a variable in the new empty object that we will return
	name = name; // this will assign a new global variable!
	var name = name; // this assigns a new variable local to the function (essentially does nothing as the argument already existed)
}

// After running the above code JS will have created 2 new objects in memory: Parent, Parent.prototype
Parent.prototype.say = function () { print("hello" + this.name); }

Every function has an ordinary object tied to it's prototype member.
This object is not special in itself but can be used in a special way if the function is a constructor

If you invoke a function as a constructor
1. An empty object gets created and passed to the function as 'this'
2. The constructor can decorate this object as it sees fit.
3. JS will create a hidden __proto__ property in the object that points at the prototype object for the constructor function.


Constructors in JS always have space allocated to them in memory - they are not templates
function Cons() {} // desugars to var Cons = function () {}





Objects are linked toghether
Each object can only have one secret link (single inheritance)
This linkage provides a simple inheritance

all objects are linked directly or indirectly to Object.prototype. Object.prototype is always the last link on the chain
all objects inherit some basic methods (none of them are very useful) e.g. hasOwnProperty(name) asks if name is a true member of this object.

changes to the old object will be reflected in the new one but not vice versa - the linkage is only one-way
JS has no built-in way to tell you if two objects contain the same stuff.

3 ways to create a new object in JS
	{ } // object literal - prefered way
	new Object()
	object(Object.prototype)

* Objects are *always* passed by reference when passed to functions.
* === compares object references, it does not compare the stuff in an object
* members can be removed with
	delete myObject[name] // changes myObject[name] value to undefined
	delete myObject.name // changes myObject[name] value to undefined

you can't create circular inheritance in JS because the __proto__ link can only link to an object that already exists and it can't be changed afterwards

js doesn't have a built-in way of telling you if two objects contain the same stuff


Wrapper objects
---------------
Java has int and Integer, two incompatible types which can both carry the same value with differing levels of efficiency and convenience.
JS copied this pattern from Java to no advantage - avoid it.
Avoid
* new Boolean()
* new String()
* new Number()

* These create objects that wrap a primitive value.
* In particular if new Boolean() is set to false, then it's value is false but it is truthy

Temporary wraper objects
-------------------------

* Wrapper objects are temporary objects created when you try to *read* a property or method on a string, boolean, number.
* These properties are *read only* as numbers, booleans, strings are all immutable in JS
* If you call a method on a string object, JS creates a new String object and wraps it around the string and then discards the object when it is finished with it (implementations are not required to actually discard the object but they must behave as if they do)
* The typeof() and === operators will show you the difference between a primitive value and it's wrapper object.

Object.create polyfill
----------------------

* In old browsers you have to polyfill Object.create:

if (typeof Object.create != "function") {
	Object.create = function (o) {
		function F() {}
		F.prototype = o;
		return new F();
	}
}


*****************************

Arrays
======

* https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/Array

Array inherits from Object
JS uses objects to *simulate* real arrays
JS arrays do not have to be dimensioned
Indexes are converted are converted into strings and used as names for retreiving values.
	* this is efficient for very sparse arrays .e.g. 1% population (those with lots of holes)
	* not efficient in most other cases. JS arrays are not particularly efficient
	* one advantage: no need to provide a length or type when creating an array
		* arrays do not have an "allocation" like they do in C
	* we can't use dot notation with arrays as it would be confusing with numbers

arrays are pretty much the same as objects like
	{
		"0" : "val1",
		"1" : "val2"
	}
except they inherit the length member and a few handy methods.


Arrays have a length member := 1 larger than the highest subscript (not necessairly the actual lenght of the array if it has holes)
.length is not necessairly equal the number of elements in the array
Arrays in JS do not have bounds because they are not really arrays, they are objects

Iterating over arrays
	Don't use for..in to iterate over an array as it will not necessairly pull the elements out in order
	Use for loop instead
		for (i = 0; i < a.length; i += 1) { }

Array literals - surrounded by [ ], values separated by commas

var ary = ['hello','there'];
ary[ary.length] = 'world'; // Adds the string at ary[2]
console.log(ary); // => hellothereworld

When to use arrays an objects
* Use objects when the names are arbitrary strings
* Use arrays with the names are sequential integers

Arrays also have a secret link - they always link to Array.prototype (you can't change this)

You shouldn't use arrays as a prototypes as the thing produced by the object() operator won't have "array nature" - it won't have the special array members (length) or methods:
	concat
	join
	sort // watch out - it will compare numbers as strings
	slice
	splice
	concat
	push
	pop

delete array[number] removes element from the array but doesn't change array numbering (it leaves a hole) - splice will do that for us.
ary.splice(1,1); // Remove one thing from the index 1 and insert nothing in it's place. splice is not fast.

you can augment an individual array by assigning a method to it
you can augment all the arrays in the sytem by augmenting Array.prototype.

How to test whether something is an object or an array
// We want to test value
value.constructor === Array // if true, then value is an array
value instanceof Array // if true, then value is an array

Manually changing the .length property
* If you make it smaller, you truncate the array
* If you make it larger it does nothing to the existing array - sure ???


The 'length' property is used by serveral of Array's built-in methods

var fruits = ['apple', 'banana', 'orange'];
fruits['eoin'] = 'test'; // works fine, just adds a new property to the array object
fruits.__proto__ === Array.prototype; // true
fruits.constructor === Array.prototype.constructor // true
Array.prototype.constructor === Array // true
typeof Array; // "function"
Array.__proto__ // function Empty() {}

* So Array is a constructor function object whose __proto__ link points at an empty function

************************************

Functions
=========

Functions grew from subroutines - subroutines were invented because we wanted:
* code reuse
* decomposition
* modularility
* expressiveness
* higher order

Recursion
----------
When you learn to think recursively you become a much stronger programmer - DC
Recursive algorightm: quicksort

1. Divide an array into two groups, low and high
2. Call Quicksort on each group containing more than one element
This is pretty fast for a sort (N logN)

JSON processing also uses recursion

Functions
---------

JS has a function operator *and* a function statement
What JS calls a function, other languages call lambda
A 'function' is another special kind of object - it has executable code associated with it. It is still an object.
A function is a 1) named and 2) parameterized block of code that you define once and then invoke repeatedly.
* In JS functions have methods
* Functions inherit from Function.prototype

Variable Hoisting
-----------------

* A variable that is declared *anywhere* within a function is visible *everywhere* within that function

function foo() {
	// do stuff
	var myVar = 0, myOtherVar
	// do other stuff
}

function foo () {
	// hoist the declaration part to the top of the function and initialize with undefined
	var myVar = undefined,
			myOtherVar = undefined;
	// do stuff
	myVar = 0; // Complete the assigment portion
	// do other stuff
}

Consider:
	function foo () {
		console.log(x); // displays 'undefined' as x has been declared (due to hoisting) and initalized to undefined
		var x = 10;
		console.log(x); // displays 10 as expected
	}
so it is good practice to put all your variable declarations at the top of the function:
	function foo () {
		var x = 10;
		console.log(x); // displays 10 as expected
		console.log(x); // displays 10 as expected
	}

Function Statement
------------------
The function statement is a shorthand for the function operator:

	function foo() {} // a function statement

expands to:

	var foo = undefined; // This gets hoisted to top of containing function
	var foo = function foo() {} // a function expression assigns the value
? is the second foo ignored?


* If the first token of a statement is 'function' then it is a function statement, otherwise it is a function expression
* A subtle consequence of this is that whenever you declare a function in JS an object gets created in memory.

? In other languages function declarations "feel" like nothing happens until you invoke them - ? is this true
? what does php/ruby interpreter do when it encounters a function? does memory get used?


Assignment oddity in JS
-----------------------

These are equivalent
var x;
var y;

// For numbers
x = 10;
y = x;
console.log(x); // 10
console.log(y); // 10
y = 11;
console.log(x); // 10
console.log(y); // 11
// => numbers are assigned by value

// For strings
x = "aaa";
y = x;
console.log(x); // "aaa"
console.log(y); // "aaa"
y = "bbb";
console.log(x); // "aaa"
console.log(y); // "bbb"
// => strings are assigned by value

// For arrays
x = [1,2];
y = x;
console.log(x); // [1,2]
console.log(y); // [1,2]
y.push(44);
console.log(x); // [1,2,44]
console.log(y); // [1,2,44]
// => arrays are assigned by reference

// For objects
x = { name: 'Eoin' };
y = x;
console.log(x); //{ name: 'Eoin' }
console.log(y); //{ name: 'Eoin' }
y.name = 'Colm';
console.log(x); // { name: 'Eoin' }
console.log(y); //{ name: 'Colm' }
// => objects are assigned by reference

x = y = 10;
// x and y are independent, changes to x will not affect y

// y is an unintended global here!
var x = y = 10;

This behaviour isnot the same for objects

var x;
var y;

x = y = {}; // both x and y point at the exact same object now! this is unlike what happens with integers
// x and y are pointing at the same thing, changes to x will affect y

x.name = 'Eoin';
console.log(y.name); // => 'Eoin'

Scope
-----

* Blocks do not have scope in JS - only functions have scope.
* variables defined in a function are not visible outside of the function

DC Recommendations
* Declare all variables at the top of the function
* Declare all functions before you call them

Functions within functions (closures)
-------------------------------------

* Sometimes called lexical scoping
* Sometimes called static scoping
* Functions can be defined inside of other functions
* An inner function has access to all the variables and parameters of the function it is contained within.
* The scope that azn inner function enjoys continues even after the parent function has returned.

random thoughts on closures
* closures let you setup some shared state and behavior for a function - sort of like a private
you want functions to be able to communicate with each other (sometimes even the same funciton in different invocations) so you need an area of shared state
you could use global vars for this but that pollutes the whole program
closures are a much cleaner way of doing this as you can explicitly share state between functions (and all invocations of a function) by declaring it within another function
you can think of the outer function as the thing that sets up the environment for the inner one
if we think of the outer function as do_task_x() then any functions declared within it allow it to split it's work up
	closures allow do_task_x to call the same chunk of code repeatedly to achieve it's goal
things that call do_task_x() don't need to know or care how it does it's job
having those internal functions only make sense if they persist beyond the life of do_task_x() - otherwise you can get code re-use from a loop or similar

declarign a function within another function says "I can't complete my task now but I am going to create chunk(s) of code that will do it. I will then schedule those chunks to run at the appropriate time."

functions within others also make sense if the task we are performing is to create a function that will perform another task at a future time - we ar ea sort of "setup function x" task. closures let us do our initialization work once and return the prepared function
=== end random stuff

* Don't make functions in a loop - it's wasteful as a new function object is created on every iteration
* It can be confusing because the new function closes over the loop''s variables not over their current values

for () {
	div_id = divs
}

Tennents Principle of Correspondance
---------------------------------

* anything you can write with variables, you can write with parameters - FIXME explain???

Y Combinator Function
---------------------
* reall complex thing, can be expressed in JS - research some other time

Immediately Invoked Function Expression IIFE
--------------------------------------------

Compare:
	var x = function () {
		// do stuff
	}();

	// The extra parens here are not *required* but they make it more obvious to the
	// reader that x is getting the result of invoking the function rather than the
	// function object.
	var x = (function () {
		// do stuff
	}());

	// This also works but DC doesn't like it as it doesn't wrap the whole invocation (which in his view is what you are trying to draw attention to)
	var x = (function () {
		// do stuff
	})();

Every function has a special [scope] property that represents the environment it was in when it was defined.
If a function is returned from another function then this reference to the old environment is closed over by the new function in a "closure"

	function a() {
		return function() { return "hello"; };
	}

A closure is a function defined within another scope that has access to all the variables within the outer scope

// Variables defined in the current scope
var x = 1;
var y = "hello";

function hello() {
	return x + y;
}

a factory function  creates a closure and exposes parts of it as public methods
so closures are an alternative way of doing encapsulation in JS

each function definition in JS creates a new "scope". A function defined within another function has a link to the "scope" of the outer function - a so called "scope chain"
an element in a scope chain is basically a map with a pointer to it's parent scope


Static members
--------------
Fuinctions are objects so they can contain name/value pairs - this can serve the same purpose as static members in other languages.

Methods
-------

Since functions are values, they can be stored in an object - if it is it is called a method

Function Arguments
------------------

if a function is called with too many arguments, the extra arguments are ignored
if a funciton is called with too few arguments, the missing values will be undefined
there is no implicit type-checking on the arguments

arguments (array-like parameter list)
-------------------------------------

The 'arguments' special parameter
* A special parameter passed to all functions - it contains all the arguments from the invocation - it delivers everything that got passed in.
* It is "an array like object" - it is not a real JS Array
* arguments.length will give you the number of arguments but other array functions do not work

* If you change one of the elements in the argumetns array, you can break your parameters. This is not the case in ES5 strict
* In practice, treat 'arguments' as read-only for sanity
* In ES5 arguments is more array like - it inherits from Array.prototype (map, reduce etc.)
* No more arguments.caller or arguments.callee in ES5 strict

4 Ways to call a function
-------------------------

There are 4 ways to call a function - they differ in what the 'this' parameter gets set to.
'this' gives methods access to their objects
'this' is bound at invocation time
Why: because this is bound at invocation time, we can re-use the function - if it performs its actions on 'this' then
the same function can be re-used e.g. a function that changes a DOM node background color could be used on *any* DOM node.

1 Function form
	functionObject(arguments)

	when a function is called in function form, it gets a special extra argument called 'this' that isbound to the global object
	In ES5 strict this will be set to undefined here, not the global object.

2 Method form
	thisObject.methodName(arguments)
	thisObject["methodName"](arguments)

	when a function is called in the method form it gets a special extra argument called 'this' which is set to thisObject (the object containing the function).
	this allows methods to have a reference to the object of interest

	If we define a helper function inside of our method then it will not be able to access the object through 'this'. To work around this we copy this into a local variable which the helper function can access (because of the rules of closure) - this is often seen as
		var that = this; // making the value of this available for the helper functions defined within this method
		// The advantage of not calling it something specific like 'domNode' is that naming would be confusing if the func was used on things other than domNodes
		// I do think the naming should be more specific than 'that'?? pros/cons?

3 Constructor form
	new functionObject(arguments)

	When a function is called with the new operator, a new object is created and assigned to 'this'.
	If the function does not have an explicit return value, 'this' will be returned
	* Used in pseudoclassical style

4 Apply form
	functionObject.apply(thisObject, [arguments]) // takes a this object and an array
	functionObject.call(thisObject, argument1, arg2 ...) // takes individual args

	* http://yehudakatz.com/2011/08/11/understanding-javascript-function-invocation-and-this/


Return Values
-------------

* Functions always return a value - if you don't specify one they will return undefined.
* The exceptions is constructors, which will return the object they are creating.

foo () {
	// do stuff

	// This will return undefined because semi-colon insertion tries to help you
	return
	{
	a: "blah"
	}
}

Constructors
------------

functions that are written to be used to initialize a new object are called contstructors.
Each constructor defines a 'class' of objects (objects that are initialized by that constructor). So classes are subtypes of the object type that are grouped together by their constructor.

Core JS defines 5 classes
1. Array
2. Function
3. Error
4. RegExp
5. Date


Function object properties exposed by chrome Dev Tools
------------------------------------------------------

Each function has the following exposed in chrome dev tools
	*	arguments
		* a local variable available within all functions
		* A special parameter passed to all functions - it contains all the arguments from the invocation - it delivers everything that got passed in.
		* It is "an array like object" - it is not a real JS Array. It does not have any array-like properties except length
		* arguments.length will give you the number of arguments but other array functions do not work
		* In practice, treat 'arguments' as read-only for sanity
		*	Properties
			* arguments.callee = a reference to the currently executing function
			*	arguments.length = reference to the number of args passed to the function
		*	https://developer.mozilla.org/en/JavaScript/Reference/Functions_and_function_scope/arguments
	*	caller
		* 	Not part of ECMAScript 3 but exists in most modern browsers. TODO All???
		*	https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/Function/caller
			returns the function that invoked the specified function
		*	If the function was invoked by top level code, caller is null
	*	length
		*	specifies the number of parameters teh function expects (the num of formally defined parameters
		*	It isn't local to the function unlike arguments.length (which tells us how many parameters we actually got)
	*	prototype
		* 	The prototype object for this function
		*
	*	name
		*	non standard
		*	read-only property
		*	the name of the funciton
		*	returns empty string for anonymous functions
	*	__proto__
		* If I put an anonymous function in an object literal it's __proto__ points to the Empty() function which is not defined according to chrome dev tools
		*	non standard and deprecated, use Object.getPrototypeOf() instead


prototype and __proto__
------------------------

When an object is created, its __proto__ property is set to the constructing function's prototype property

All functions (function objects) in JS have a prototype property.
If that function is used as a constructor (using the new keyword) then JS will make the __proto__ of the new object point to the prototype of the constructor function
__proto__
	is a property of instances
	is used for prototypal inheritance
	is used at runtime to look up properties not in the object

prototype
	is an object
	is a property of all functions (function objects) in JS
	has a constructor property (prototype.constructor) that points to the constructor function
	is unused in functions that are not called with the new keyword (i.e. not constructors) - TODO is this true???
	if a function is called with the new keyword (used as a constructor) then the object it creates will have a __proto__ property that points at the prototype of the constructor. This means that anything in the prototype property of the constructor is available to all objects created by the constructor.
	They share access to it so if the prototype changes during runtime then it is changed for all objects created either before or after the change
	prototypical object, an object used as a template from which to get the initial properties for a new object
	awesome: http://stackoverflow.com/questions/1646698/what-is-the-new-keyword-in-javascript
	https://developer.mozilla.org/en/JavaScript/Guide/Details_of_the_Object_Model
	http://joost.zeekat.nl/constructors-considered-mildly-confusing.html

	the 'prototype' object is the part of the constructor that is shared with all objects created by invoking that constructor with the new keyword - it's kind of the 'payload'

	In JS functions are constructors depending on how they are invoked (wth the new keyword or not) unlike other languages where they have a special name e.g. __constructor__
	A "constructor" in JavaScript is "just" a function that happens to be called with the new operator.


When you create an object literal, its __proto__ points to Object


https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/Object/Proto

Each object literial I create has
	__proto__ which points to 'Object' which is a function


Passing Arguments to functions in JS efficiently
------------------------------------------------

*	In JS numbers, booleans, null, undefined, strings are all immutable. objects and arrays are mutable.
*	JS passes primitive values to functions by value and passes objects by reference:
	http://snook.ca/archives/javascript/javascript_pass/
		When passing in a primitive type variable like a string or a number, the value
		is passed in by value. This means that any changes to that variable while in
		the function are completely separate from anything that happens outside the
		function. Passing in an object, however, passes it in by reference.

in JS is there a perf advantage to passing multiple primitve values rather than one object
	function myfun1({ var1: 1, var2: 2}){}
	vs
	function myfun1($var1, $var2);

+	it is easier to understand the code if we pass named parameters
- it takes longer to type

the first one is the equivalent of createing an new unnamed object literal and passing a reference to it into thta function
that function will operate on that exact object, not a copy

myfun1(1,true)
vs
myfun1({numer: 1, doOtherStuff: true})


eval() (powerful & dangerous)
-----------------------------

the eval function compiles and executes a string in the context of the function it's called in i.e. it tivs you access to the javascript compiler and interpreter . Crockford recommends not using eval() ever.
It is what the browser uses to convert strings into actions
eval() is a slow way of doing whatever you want to do - it's just a bad idea - it makes it easier to ship bugs
the only time it is correct to use eval() is with JSON and then only if you can *absolutely* trust the server to have good intentions and not incompetent - otherwise use parseJson() method
eval() is in the language because the browser uses it

Function constructor
--------------------
new Function(parameters, body)
	var negate = new Function('a', 'return -a;');
	var adder = new Function('a', 'b', 'return a + b;');

* The Function constructor takes 0 or more parameter name strings and a body stirng and used the javascript compiler to produce a function object
* It should bonly be used to compile fresh source from a server
* The browser uses eval when it finds an event handler or a script tag - the language designers exposed it to us too
* It is closely related to eval() - eval calls this. Don't use this for the same reasons we don't use eval()

Understanding binding functions to context
------------------------------------------

function Greeter (s) {
    this.salutation = s;
}
Greeter.prototype.greet = function (name) {
    console.log(this.salutation + name);
}


function Server (n) {
    this.name = n;
}
Server.prototype.getUser = function (greeterFunc) {
    greeterFunc(this.name); // We are invoking this function with 'this' === window
}

var greeter = new Greeter('yo yo yo!');
var server = new Server('localhoe');

// We pass the function object 'greeter.greet' which is invoked within getUser.
// However when getUser invokes it, 'this' is set to server.

server.getUser(greeter.greet); // => undefinedlocalhoe

// Here we pass an anonymous function object which contains greeter in it's
// closure. when greet is invoked it is invoked as a method of greeter which
// sets 'this' to greeter (this is set at call time)
server.getUser(function(name){greeter.greet(name);});

// We would like to be able to control what a function gets as 'this'
// Sometimes it's a pain that we aren't sure what 'this' will be in a function. In the example above we want 'this' in greet() to always refer to the greet object. We can create a bind() function that takes a desired 'this' object and a function and calls the function with the 'this' object set. It's sort of an intercept that "fixes" 'this' for us before the function is invoked

function bind(fnThis, fn) {
    // return a new function (created by apply()) that always invokes fn with this = fnThis
    return fn.apply(fnThis, arguments);
}

// Now we can use it in our example above
server.getUser(bind(greeter, greeter.greet));

A string reverse function
-------------------------

// Only extend built-in prototypes if you are happy that you will control *all* the code in the app!

String.prototype.reverse = function () {
    var text = '';
    for(var i = 0; i < this.length; i++) {
        text = this.charAt(i) + text; // prepend the character found at index i to the text variable
    }
    return text;
}

************************************

Misc Stuff
==========

Namespaces
----------
* Every object in JS is a separate namespace - can use objects to organize your variables and functions e.g. YAHOO object
* recomment to pick an all uppercase name as a global object e.g. ALLIB

Encapsulation
-------------

* Function scope can create an encapsulation-variables defined inside a function are not visible outside
* We can use an anonymous function to wrap an encapsulation



Javascript has no cast operator - it traded type-safety for dynamism

Why does we have inheritance in any language?
* Automatic casting - not really relevant in JS as we don't cast
DC contends that a type system where you have to explicitly cast things doesn't give you type safety
* Code reuse

Date
----

* JS has a Date object

Regular Expressions
-------------------

JS has regular expressions
	* reg exp literals are enclosed in slashes (you can't use different delimters as you can in other langs)
		var x = /^hello$/;
	* reg exps scare DC

Threads
-------

The language definiton is neutral on thrads
most applications do not provide threads but spidermonkey can do them
DC thinks they are evil and that they don't belong at the application level

Global variables (The global object)
------------------------------------

*	Global variables in JS are actually *properties* of the 'global object'
* Variables declared without var are *always* global even if declared in a function.
* What this is depends on the context that the JS is running e.g. in a web browser the global object is window. You can get/set variables in the global object with window.varname
* This means you can access global variables from one window in another window or frame by specifying the other window or frame's *name*.

The global object contains:
* global properties like: undefined, Infinity, NaN
* global functions like: isNaN(), parseInt(), eval()
* constructor functions like Date(), RegExp(), String(), Object(), Array()
* global objects like Math, JSON

In client-side JS the Window object serves as the global object for all JS code contained in the browser window it represents. It has the self referential 'window' property which can be used to refer to itself.

* There is no direct way of geting at the global object in JS
* Sometimes 'this' points to it
* On browsers, 'window' is the global object - it is not directly accessible but you can get at it by a work-around
		They assigned to the global object a window member whose value is the global object
		window.x -> goes to the global object, finds window member, which then gives you the global object back

Global variables are evil:

* Functions within an application can clobber each other
* Cooperating applications can clobber each other
* Use of the global namespace must be minimized

The implied global:

* Any var which is not properly declared is assumed to be global by default
* Intended to make it easy to program
* Use JS lint to help identify implied globals and other weaknesses


Operators
=========

. (property operator)
---------------------

The . operator is called the property operator - use it to access the methods and data of an object.
Oddly any whitespace on either side of it is ignored
var obby = { name: 'Eoin' };
obby    .    age = 33; // works fine!


in
--

* returns true if the specified property is in the specified object
* it checks the property name not the value!
* it will check property names up the prototype chain

var person = { name: "Eoin"; age: 33; }
name in person // => true
"Eoin" in person // => false
"toString" in person // => true (found it in Object.prototype)

% (remainder operator)
---------------------

-1 % 8 // => -1

% is the remainder operator, not the modulus operator - this is an important distinction FIXME explain

+ (unary operator)
------------------

* The + operator is special - in a statement involving the + operator javascript will automatically convert numbers to strings
* used for addition and concatenation (it's overloaded) - this is a bad idea.
* if both operands are numbers it adds them, otherwise it converts both operands to strings and concatenates them - DC believes this is a mistake!
* can also be used as a prefix operator which converts a value to a number
	* +value is equivalent to Number(value)
	* +"42" = 42
	* Number("42") = 42
* be careful that when you intend to add, that both operands are numbers

equality operators (== and !=)
------------------------------

* can do type coercion - careful now!

=== and !== (the strict equality operators)
-------------------------------------------

* checks for exact equality of value *and* type

&& (guard operator/logical AND operator)
----------------------------------------

* if the first operand is truthy, the result is the second operand, otherwise it returns the  first operand
* NB it doesn't necessairly return true or false!
* It can be used to avoid null references

|| (default operator/logical OR operator)
-----------------------------------------

* if the first operand is truthy, the result is the first operand, otherwise the result is the second operand
* NB it doesn't necessairly return true or false
* It can be used to fill in default values

! (lobical NOT operator)
------------------------

* If the operand is truthy the result is false and vice versa
* !! will always produce a boolean value

bitwise oeprators: << >> >>> & | ^
----------------------------------

* take the 64 floating point number, convert it to a 32 bit integer, do the operation and then convert it back to a 64 bit floating point number
* makes thse operators slower than in languages like C

Comma Operator
--------------

The comma operator (,) simply evaluates both of its operands and returns the
value of the second operand. This operator is primarily used inside a for loop,
to allow multiple variables to be updated each time through the loop.

	var x = 10, y = 20; // returns undefined (the assignment operation returns undefined)
	x = 33, y+10; // returns 30

It can be used in for loops like

	for (var i = 0, j = 9; i <= 9; i++, j--) {}

The comma operator has the lowest precedence of all operators.

It has left-to-right associtivity meaning that
	var x = 1, y = 2, z = 3;
is evaluatea as
	(x = 1, y = 2) , z = 3;

(  ) suffix operator
-------------------
* Causes function invocation
* Surrounds 0 or more comma separated expressions arguments
* Those arguments will be *bound* to the parameters of the function

Operators in loops
------------------

	var len = 30,
		x = 1;
	while (x < len) {
		// do stuff
		x = x + 1;
	}

The condition here has 2 steps
1.	Evaluate the expression  x < len
2. Evaluate whether the result of step 1. === true or not

for
---

for ([initialization]; [condition]; [final-expression])
	statement

initialization = an expression run before the loop begins
	* the result of this expression is thrown away
	* variables in the expression are not local to the loop!

condition
	* an *expression* to be evaluated before each loop iteration
	* if the expression evaluates to true, statement is executed
	* if the expression evaluates to false the loop is exited

final-expression
	* An expression to be evaluated at the end of each loop iteration *before* the next evaluation of condition
	* you can imagine this being pasted in at the end of the code in the loop

the execution path for a loop that goes through 3 iterations is
[initialization]

[condition]
[statement]
[final-expression]

[condition]
[statement]
[final-expression]

[condition]
[statement]
[final-expression]

[condition] // evaluates false so we exit the loop

for .. in
---------

for (variable in object) {
	// You will get properties from the object and any object in it's inheritance chain
}

for (variable in object) {
	if( obj.hasOwnProperty(variable) ) {
		// Here you will not get any properties from the inheritance chain
	}
}

* https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Statements/for...in
* Iterates over the enumerable properties of an object, in arbitrary order. For each distinct property, statements can be executed.
* for...in does not iterate over non-enumerable properties of the object e.g. Object.toString
* it iterates over the properties of an object in *arbitrary* order
* In general it is best not to add, modify or remove properties from the object during iteration, other than the property currently being visited. There is no guarantee whether or not an added property will be visited, whether a modified property (other than the current one) will be visited before or after it is modified, or whether a deleted property will be visited before it is deleted.
* Do not use for...in on an Array - there is no garuantee that it will return the indexes in any particular order - use a for loop instead
* for..in does a deep dredge of the object and it's inheritance chain - you have to filter out properties from parent objects yourself.
* for..in is not as fast as a for loop

? what are the best practices for finding out stuff about an object in JS
	what methods it has
	what variables it has
	what it's prototype object is

foreach
-------

* JS has no foreach statement
* There is an Array.forEach(callback) that you can use to iterate over an array
* Not natively in IE 6-8, ajax libs mostly have a version
* https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/Array/forEach
foreach is not as fast as for ??? verify this

typeof
------

The typeof() operator returns:
string		'string'
number		'number'
boolean		'boolean'
null			'object' (Watch out!)
undefined	'undefined'
object		'object'
array			'object' (Watch out!)
function	'function'

The spooky typeof operator returns 'undefined' when its operand is a simple variable that does not exist, instead of throwing an error as would normally happen if you tried to refer to it. (You can also give it a simple variable wrapped in parentheses, but not a full expression involving a non-existant variable.

instanceof
----------

* https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Operators/instanceof?redirectlocale=en-US&redirectslug=Core_JavaScript_1.5_Reference%2FOperators%2FSpecial_Operators%2Finstanceof_Operator

Usage:
	object instanceof constuctor

	var foo = {};
	foo instanceof Object; // true, tests whether 'Object.prototype' exists somewhere on foo's prototype chain

* The instanceof operator tests presence of constructor.prototype in object prototype chain.

function Cool(){} // A constructor for Cool objects

var sinatra = new Cool();
sinatra instanceof Cool; // => true
sinatra instanceof Object; // => true

var ob1 = {};
ob1 instanceof Object; // => true
// We are asking "does ob1 have Object.prototype in it's prototype chain"

function C(){}

var o = new C();
o instanceof C; // true
C.prototype = {}
var o2 = new C();
o2 instanceof C; // true
o instanceof C; // false because now C.prototype isn't in o's prototype chain anymore

block statements
-----------------

A 'block statement' is used to group statements and delimited by a pair of curly braces.

* JS does not have a 'block scope'. Any variables introduced within a block are scoped to the containing function or script and the effects of setting them persist beyond the block itself. Blocks are used as part of control structures (for, while etc.)
* You *can* use them stand-alone but they don't really do anything so don't do this (they do stuff in C/Java)

{
	var e3 = 'hello';
	var e31;
	console.log('i\'m a mothereffin block');
}

{ var small_block = 'hi there'; } // I'm a block too


this
----

* The this parameter contains a reference to the object of invocation
* this allows a method to know what object it is concerned with
* Allows a single function object to service many functions
* this is key to prototypal inheritance

All methods of invoking a function in JS are sugars on func.call(thisValue, param1, param2 ... )
You can invoke a funciton with .call if you need to control the this value

.apply is just a verision of .call that takes an array of arguments as 2nd param rather than separate params - this is more flexible

function hello(thing) {
	console.log(this + ' says hello ' + thing);
}

hello('foo'); // [window object] says hello foo
hello.call('eoin', 'foo'); // eoin says hello foo
hello.apply('eoin', ['foo']); // eoin says hello foo

a function does not have a persistent notion of 'this' - it is set at call-time

hello(args); // this = window
'use strict'; hello(args); // this = undefined

DC recommends using the function form of strict mode as it won't break other code that isn't strict


// Check if you are in strict mode
function in_strict_mode() {
	return (function () {
		// If this === window then we return false, if this === undefined we return true
		return !this;
	}());
}

// See if strict mode is possible
function strict_mode_implemented() {
	return (function () {
		'use strict';
		// If strict pragma worked this should be undefined so func will return true, otherwise false
		return !this;
	}());
}


myobj.hello(args); // this = myobj
new ObjConstructor(); // this = {}

************************************

Programming Style
=================
According to DC:
* It's not about personal taste
* It is about rigor in expression
* It is about clearness in presentation
* It is about product adaptability and longevity
* Good rules help us to keep the quality of our programs high

Style is particularly important in JS - it is "soft", loosely typed, design errors etc. it's easy to get confused

Code conventions for Javascript
-------------------------------

Parentheses
* No space before ( when used to invoke a function
* No space between a function name and a parameter list
* Once space between all other names and (

wrong:
	foo (bar);
	return(a+b);
	if(a===0) {}
	function foo (b) {}
	function(x) {}

correct
	foo(bar);
	return (a+b);
	if (a===0) {}
	function foo(b) {}
	function (x) {}


DC likes:
(function () {

}());
DC does not like:
(function () {

})();	// he thinks it's not as clear to the future programmer what our *intent* is.

When the compiler sees an error, it attempts to replace a nearby linefeed with a semicolon and try again - always use full, correct forms, including semicolons

Defence against certain errors:
	Always break a line after a punctuator
	Never break a line after a name, string, number ) ] ++ --

Don't use extra commas in array literals - diff browsers will give diff array length if you have a traling comma in an array literal e.g.
var a1 = [2,3]; // a1.length = 2 in Mozilla & IE
var a1 = [2,3,]; // a1.length = 2 in Mozilla & a1.length = 3 in IE

Always use { } around blocks
Only use blocks for structured statements - JS has no block scope. The structured statements are:
* function
*	if
* switch
* while
* do
*	for
*	try

Define all variables at the beginning of the function - they will end up here anyway due to hoisting so you may as well be explicit.
* also recommends declaring functions before you call them because they are just created as variables and hoisted anway

DC recommends making global variables all UPPERCASE as a warning
Expression Statements
Any expression can be used as a statement - this can mask errors.
Only assignment expressions and invocation expressions should be used as statements

Good:
foo(); // invocation statement
var a = b; // assignment statement

Bad:
foo && foo(); // better to control flow here as an if

avoid fallthrough in switch statements - every claus should explicitly break/return/throw
do not use assignment in the conditional part of if, while - others will not assume you got it right
Be aware that == and != do type coercion
only put labels on statements that make sense :for, do, while
*	avoid using javascript:// urls

JSLint finds all these things

Functional Programming in JS
============================

http://matt.might.net/articles/by-example-continuation-passing-style/
http://underscorejs.org

A pure function is one that has no side effects - it only acts on it's inputs and returns a value

why do we use loops?
	to enumerate lists - go through each element take an action with each item
	extracting stuff from lists
	aggregating list contents (boiling down a list to a single value)
	filtering
	transformation
	combining items
	side effects

// arr is an array
var arr = [];

for (var i = 0, l = arr.length; i < l; i++) {

}

arr.forEach(function (item) {
	console.log(item);
})

a map is a special loop that allows me to transform a list of data into another list of the same length

