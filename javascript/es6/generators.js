function* helloWorldGenerator() {
  yield 'hello';
  yield 'world';
}

var helloWorldIterator = helloWorldGenerator();

console.log(helloWorldIterator); // { next: [Function], throw: [Function], return: [Function] }
console.log(helloWorldIterator.next()); // prints { value: 'hello', done: false }
console.log(helloWorldIterator.next()); // prints { value: 'world', done: false }
console.log(helloWorldIterator.next()); // prints { value: undefined, done: true }

// Normal JS functions are "run to completion" i.e. a function will always
// complete in its turn of the event loop.
//
// Generators are "cooperative" concurrency
// A generator is a function that can yield control back to the caller
// when it does so, the caller has to restart it
// values can be passed out to the caller when it yields
// values can be passed back in when it is resumed

// A generator is a special type of function that works as a factory for
// iterators. A function becomes a generator if it contains one or more yield
// expressions and if it uses the function* syntax.
//
// it does not matter where you put the * (but against the function name is preferred)
function* foo() {}
function* foo() {}

// yield XYZ
// * is an expression
// * XYZ is passed out of the function
// * whatever is passed in on resumption becomes the value of the expression
// * you can think of "yield" as a request for a value
//
// calling a generator in the normal way does NOT execute it, it simply returns the iterator which can be used to run the generator

function* reallySimpleGenerator() {
  yield 1;
  yield 2;
  yield 3;
  yield 4;
  yield 5;
}

var iterator = reallySimpleGenerator();

var thing = iterator.next();
console.log(thing); // { value: 1, done: false }
console.log(iterator.next()); // { value: 2, done: false }
console.log(iterator.next()); // { value: 3, done: false }
console.log(iterator.next()); // { value: 4, done: false }
console.log(iterator.next()); // { value: 5, done: false }

// note that done is false above even though we have yielded for the last time.
// The generator isn't done until the result of (yield 5) is computed and
// passed back into it

console.log(iterator.next()); // { value: undefined, done: true }

function* generatorWithReturn() {
  yield 1;
  yield 2;
  return 3;
}

// When you add an explicit return statement to a generator it will be returned
// by next() the first time that done is true
//
// Note that if for..of is using the iterator then the returned value is thrown
// away so it might be a confusing pattern to use
var iterator2 = generatorWithReturn();
console.log(iterator2.next()); //{ value: 1, done: false }
console.log(iterator2.next()); //{ value: 2, done: false }
console.log(iterator2.next()); //{ value: 3, done: true }
console.log(iterator2.next()); //{ value: undefined, done: true }
console.log(iterator2.next()); //{ value: undefined, done: true }
