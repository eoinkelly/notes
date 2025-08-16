Options

## for

- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/for
- all browsers support it
- works as in other langs
- GOTCHA: any variables declarations delcared with `var` in the initialization
  block will be hoisted to top of function
- allows variables to be declared in the loop head
    - var: creates a single binding for the variable
    - let: creates a new binding for each loop iteration
    - const: works like var but you can't change the binding

```js

```

## for..in

- iterates over _enumberable_ properties (has nothing to do with the new
  "iterator hook")
- allows variables to be declared in the loop head (for..in and for..of treat
  var|let|const the same)
    - var: creates a single binding for all iterations
    - let: creates a new binding for each iteration
    - const: creates a single immutable binding for all iterations (can think of
      it like an immutable `var`)
- https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Statements/for...in
- available in all browsers (since IE6)
- you cannot use initializer expression with this loop e.g.
  `for(var x=0 in obj) {` is illegal
- will also walk up the prototype chain to get enumerable properties
    - you can use `Object.prototype.hasOwnProperty()` check to filter for only
      the properties in the first object
- interates in arbitrary order
    - GOTCHA: don't user for..in to iterate over an array and expect to get
      things in index order. Instead use
        - Array.prototype.forEach()
        - for

## for..of

- new in ES2015
- will iterate over "iterable objects" by invoking the `[Symbol.iterator]()`
  iteration hook
- built-in JS types already implement this hook: Array, Set, Map etc.
- allows variables to be declared in the loop head (for..in and for..of treat
  var|let|const the same)
    - var: creates a single binding for all iterations
    - let: creates a new binding for each iteration
    - const: creates a single immutable binding for all iterations (can think of
      it like an immutable `var`)
