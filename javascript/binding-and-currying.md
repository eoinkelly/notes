# Functional vs OO decomposition

## bind

- bind() is JS is just sugar for making a wrapper function
- it allows you to create a version of a a function that always uses the same
  receiver (no matter how it is called)

```js
var foo = function () {
    return this.a + this.b;
};
var ob = {};

var obFoo = function () {
    return foo.call(ob);
};

// * obFoo is a specialisation of foo that is "bound" to ob
// * it is a version of foo that doesn't care about how you call it
//   because it ignores 'this'
```

## Currying

- currying is a method of making specialised copies of functions that have some
  args hard coded.
- it can be achieved with wrapper functions in JS
- you can use bind to save on some typing
