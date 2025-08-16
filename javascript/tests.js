// Immediately executing function expression
// (function (){
// 	"use strict";
// 	// contains all variables declared with var

// }());

/*
Self executing anonymous function
This immediately executing function is a popular pattern because:
*   By wrapping our code in a function, we can create variables without putting them
    in the global scope.
*   Safe 'undefined' value:
    We call our function with 2 args so the 3rd is garuanteed to be undefined. This
    protects us from some other code on the page messing with the value of 'undefined'
    e.g. undefined = true; would be a Bad Thing(TM)
*   Because we are passing 'window' and 'document' as parameters, we get a slight
    benefit when we minify as the minifier can safely replace them with shorter
    strings e.g. aa, bb.
*   The is a very slight performance win by the VM not having to traverse up to
    global scope to get values for 'window' and 'document'.
*   More info: http://www.youtube.com/watch?v=i_qE1iAmjFg
*/

//IIFE that also gives you a garuanteed OK undefined + a few small minification & perf wins
(function (window, document, undefined) {
  'use strict';
  // contains all variables declared with var
})(this, this.document);

// // *************************************************************************** *
// // Running Functions at intervals
// // ************************************************************************I** *

// // Run the function every 100mS
// // Downside is that it can get backed up if doStuff() takes longer than 100mS
// // this can mean that we get queued up executions of doStuff() that don't return
// // in a particular order
// var intervalID = setInterval(function(){
// 	// this = ???
//     doStuff();
// }, 100);

// // window.clearInterval
// // window.setTimeout()
// // window.setInterval()
// clearInterval(intervalID); // Cancel the interval

// /*
// This pattern is superior because it works ok if doStuff() takes longer thant 100ms
// *   It leaves a gap of 100mS between each call to doStuff() rather than calling
//     doStuff() every 100mS
// */
// (function loop(){
//     doStuff();
//     setTimeout(loop,100);
// })();

// // *************************************************************************** *

// // Differentiate between an object and an array
// var x = { a: 33 };
// var y = [1,2,3];
// Array.isArray(x); // returns false
// Array.isArray(y); // returns true

// // THese should be my best practice way to do things in JS

// // while, do, for, switch can all have a label prefix that you can target with break <prefixname>;
// // Basic Loops
// while (expression) { // if expression is falsy, the loop will break, if truthy the block will be executed

// }

// // similar to while but block alwasy executed at least once
// do {

// } while (expression);

// // for (initialization, condition, increment)
// for (var i = 1; i < 100; i++) {

// }

// // ANy statement in JS can have a label and the 'break' keyword can be given that label and told to break that statement

// outer: while (1) {
// 	inner: for (var x = 1; x < 100; x+=1) {
// 		console.log(x);
// 		if (x === 30) {
// 			break outer;
// 		}
// 	}
// }

// // how to loop through the things in an object
// for (var myvar in obj) {
// 	// We use hasOwnProperty() to stop JS going up the prototype chain
// 	if (obj.hasOwnProperty(myvar)) {
// 		// ...
// 	}
// }

// // Multiway branch (switch)
// var foo = 4;

// // Never let execution fall through cases - it's bad practice
// switch (expression) { // expression can be anything that produces a number or string
// 	case 1:
// 		console.log('hello');
// 		break;
// 	case 3:
// 		console.log('hello');
// 		break;
// 	default:
// 		console.log('nothing matched');
// 		break;
// }

// try {
// 	// stuff
// 	if (something) {
// 		// Note that you don't have to throw an Exception object (but usually will)
// 		throw "A bad thing happened";
// 	}
// } catch (e) {
// 	// e = the thring that was thrown
// 	console.log('caught an error');
// }

(function (window, document, undefined) {
  'use strict';
  // contains all variables declared with var

  var l1 = 1;
  // Desugars to var hello = function () {}
  function hello() {
    var l2 = 2;
    console.log('hell');
    function another() {
      var l3 = 3;
    }
  }

  /*

Crockford - The Javascript Programming Language - Video 3
---------------------------------------------------------

*	If a func is called with too many args they are ignored
*	If a func is called tiwth too few args, the others get set to undefined

There are 4 ways to call a function

1.	Function form
	functionObject(arguments)
	'this' points at the global object

2.	Method form
	thisObject.methodName(arguments)
	thisObject["methodName"](arguments)

	'this' points to the object it was called on

3.	Constructor Form
	new functionObject(arguments)
	a new object is created and assigned to 'this'
	if the function does not explicitly return a value, the new object is returned

4.	Apply form
	functionObject.apply(thisObject,[arguments])

*	Functions also get an "array like" object called 'arguments' when they are called.
	*	All parameters passed to function will be in there
	*	'argumetns' is not a real array, it is an "array like object" - many array functions do not work

eval() actually does this:
	new Function(parameters, body)
	*	new Function is a special JS thing. Don't use it for the same reason we don't use eval()

*	JS does wrap basic types with objects e.g.
	*	new Boolean() // The value of this is false but it's value is truthy. W.T.F.
	*	new String()
	*	new Number
	It creates an object which wraps a basic value which is not the same as the basic value
	DC recommends not to use these at all.

JS has no linker - it uses the global object
*	In web browsers, the global object is the window object by adding a window member to
	the global object that points to the global object

	This means
	document.foo is faster than
	window.document.foo is slower as it has to do more lookups

*/
})(this, this.document);
