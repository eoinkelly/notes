# Generators

Transpiling generator syntax to es5

1. http://facebook.github.io/regenerator/
    * used by babel
2. google traceur

node 0.12 still only has them behind a flag
io.js 1.3.0 has pretty good but not complete support
babel has better support

* all generators are iterators
* a generator is a special "routine" that is used to control the iteration behaviour of a loop
* generators _return_ iterators

* A generator is very similar to a function that returns an array, in that a generator has parameters, can be called, and generates a sequence of values.
* However, instead of building an array containing all the values and returning them all at once, a generator yields the values one at a time, which requires less memory and allows the caller to get started processing the first few values immediately.
* In short, a generator looks like a function but behaves like an iterator.

History

* old concept
* originally appead in CLU language
* already in Python, C#, and Ruby, Lua
* In ruby the Enumerable interface is a generator
* Java has iterators but not geneators
* everything is a generator in Haskell
* C++, Perl have modules for them

## iterator

* an object that assists in traversing a collection (or "container") (note that a container is a very general term - not just arrays, but hashes, trees etc. etc.)
* they usually expose a `.next()` function that is used to get the next element in the container

In JS
next() returns an `NextResult` object that contains the properties
    * `value` (the value)
    * `done` a flag indicating whether the generator is complete or not

# run to completion

JavaScript has run-to-completion execution semantics. What this means, more-or-less, is that once a task begins, it runs until it's complete. Unlike systems that rely on preemptive scheduling (such as threading), there is nothing in the JavaScript runtime that will preemptively pause the execution of a given task, permit some other code to execute for awhile, and then resume the original task.

The "run-to-completion" terminology comes from the guarantee that a task will complete without being preempted. Therefore, if we actually do want a task to take a "timeout" and let some other code run for awhile, we use the event-loop. This type of multi-tasking is said to be "cooperative", as the currently executing task has complete control until it voluntarily gives it up.

consequences:
    * JS has no way of "interrupting" the currently running task to do something else
    * JS does a sort of "cooperative multitasking"
    * JS provides "non blocking IO" by spliting up IO into a task that reqquests the stuff and a task that gets the value. Both of these are "run to completion" but in between the CPU is available

```js

function* helloWorldGenerator() {
        yield 'hello';
            yield 'world';
}

var hw = helloWorldGenerator();
console.log(hw); // { next: [Function], throw: [Function], return: [Function] }
console.log(hw.next()); // prints { value: 'hello', done: false }
console.log(hw.next()); // prints { value: 'world', done: false }
console.log(hw.next()); // prints { value: undefined, done: true }
```
