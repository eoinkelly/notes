# async await

* Syntactic sugar on top of promises
* The feature is made up of:
    * async **functions**
    * await **keyword**
* async functions always return a promise
* the `async` keyword causes JS to wrap whatever value you return in a promise (you don't need to wrap it)
    * Q: how can i control the promise I return e.g. Promise.all stuff?
* the `await` keyword
    * causes JS to pause your code on that line until the promise settles
    * your code finishes it's turn on the event loop
    * Once the promise settles it either
        * continue on with the fulfilled value
        * or `throw` the rejected value
    * you can use `await` on any code which returns a promise (even if that code isn't async aware)
    * can be used in
        * within `async` functions
        * at the top level of a module in Chrome only

```js

// "hello" gets converted to a promise which immediately resolves to "hello"
await "hello";

```

You can handle errors from awaited functions by either wrapping them in `try...catch` or chaining `.catch(...)` on to the function call

* Top level await
    * it means JS has to pause the evaluation of the module code itself so it delays module loading
    * a module which depends on other modules must wait for the child modules to be loaded before it can finish loading too
