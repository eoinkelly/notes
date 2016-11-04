# Generators

## Use cases

* I'm unsure tbh
* they seem to exist in part to enable `async` and `await`

## About

* a generator returns an iterator when it is run. From then on you work with the iterator
* generators are just a factory for an iterator
* the iterator is a thing which can be re-run as often as possible and will generate values
* it keeps its own state between runs
* it can be approximated with a closure returning an object but it is a bit clumsy

Getting value out of the generator

* `yield someValue` is an expression that pauses the generator and can send a value out of it
* the value comes out wrapped in an object of the form `{ value: "theValue", done: false }`

Pumping values into the generator

* `iterator.next(someIncomingValue)`
* next is used to run the generator until the next yield


=======================

* generators are a way of making streams
* if you work with a generator you are working with a stream where you pull from it
* A generator is very similar to a function that returns an array, in that a
  generator has parameters, can be called, and generates a sequence of values.
* However, instead of building an array containing all the values and returning
  them all at once, a generator yields the values one at a time, which requires
  less memory and allows the caller to get started processing the first few
  values immediately.
* In short, a generator looks like a function but behaves like an iterator.

## History

* old concept
* originally appead in CLU language
* already in Python, C#, and Ruby, Lua
* In ruby the Enumerable interface is a generator
* Java has iterators but not geneators
* everything is a generator in Haskell
* C++, Perl have modules for them

## iterator

* an object that assists in traversing a collection (or "container") (note that
  a container is a very general term - not just arrays, but hashes, trees etc.
  etc.)
* they usually expose a `.next()` function that is used to get the next element
  in the container

In JS `next()` returns an `NextResult` object that contains the properties

* `value` (the value)
* `done` a flag indicating whether the generator is complete or not

# run to completion

JavaScript has run-to-completion execution semantics. What this means,
more-or-less, is that once a task begins, it runs until it's complete. Unlike
systems that rely on preemptive scheduling (such as threading), there is
nothing in the JavaScript runtime that will preemptively pause the execution of
a given task, permit some other code to execute for awhile, and then resume the
original task.

The "run-to-completion" terminology comes from the guarantee that a task will
complete without being preempted. Therefore, if we actually do want a task to
take a "timeout" and let some other code run for awhile, we use the event-loop.
This type of multi-tasking is said to be "cooperative", as the currently
executing task has complete control until it voluntarily gives it up.

consequences:

* JS has no way of "interrupting" the currently running task to do something else
* JS does a sort of "cooperative multitasking"
* JS provides "non blocking IO" by spliting up IO into a task that requests
  the stuff and a task that gets the value. Both of these are "run to
  completion" but in between the CPU is available

```js
function* helloWorldGenerator() {
  yield 'hello';
  yield 'world';
}

var iterator = helloWorldGenerator();
console.log(iterator); // { next: [Function], throw: [Function], return: [Function] }
console.log(iterator.next()); // prints { value: 'hello', done: false }
console.log(iterator.next()); // prints { value: 'world', done: false }
console.log(iterator.next()); // prints { value: undefined, done: true }
```
