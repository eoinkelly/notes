# Fat arrow functions

Fat arrow functions are in a sort of "everything lexical" mode - `this` and
`arguments` (the two context specific vars) are **both** lexically bound!

- https://www.nczonline.net/blog/2013/09/10/understanding-ecmascript-6-arrow-functions/
- https://rainsoft.io/when-not-to-use-arrow-functions-in-javascript/
- http://exploringjs.com/es6/ch_arrow-functions.html

The complete list of variables whose values are determined lexically in arrow
functions is

1. arguments
2. super
3. this
4. new.target

Notes on arrow functions

1. Lexical `this` binding - the value of `this` is determined by where the
   function is defined not by where it is used.
1. You cannot change the value of `this` within the function
    - allows JS engines to optimize the function better
    - this rule is what prohibits you from using arrow functions as constructors
      (constructors mutate their `this`)
1. You cannot use arrow functions with `new` (i.e. as a constructor)
    - allows JS engines to optimize the function better

- `this` is lexically bound when using a fat arrow function
- nested fat arrow functions will share the value of `arguments`
    - Aside: I'm not sure what value lexical `arguments` would have so probably
      best to avoid
- `typeof` an arrow function is `"function"` just like regular functions
- arrow functions are instances of `Function` so `instanceof` works as expected
- `call()`, `apply()`, `bind()` still work with arrow functions but you can't
  change the value of `this` with them

When not to use them

All these boil down to `this` binds **statically** in arrow functions so it will
be set to whatever **function** you are currently in (or `window` if you are
lexically at top level)

- don't use arrow functions to define a method on an object you aren't lexically
  inside a function so `this` will be `window` (or whatever it is for the
  lexical function you are within)
    - use ES6 shorthand method syntax instead
- don'e use arrow functions to add functions to an objects' prototype you have
  the same problem as above
    - use an old school `function` instead
- don't use arrow functions to bind listeners to DOM events
- don't use arrow functions to make a constructor (you can't anyway)

## Syntax

```js
let doubler = x => x * x;

// Notice that implicit return does not work if you use {}
let doubler_2 = x => {
    x * x;
}; // returns undefined
let doubler_3 = x => {
    return x * x;
};

// If you have no args you need to use () on LHS of =>
// let logger = => console.log("hi") // syntax error
let logger = () => console.log('hi');

// If you have more than 1 arg you must wrap args in parens
//let thing_1 = a,b => a + b // syntax error
let thing_1 = (a, b) => a + b; // syntax error

// If you want to use the super short syntax and return an object,
//use () around the function body
// let thing_2 = x => {thing: x } // syntax error
let thing_2 = x => ({ thing: x });
```

```js
'use strict';

var doubler = function doubler(x) {
    return x * x;
};

// Notice that implicit return does not work if you use {}
var doubler_2 = function doubler_2(x) {
    x * x;
}; // returns undefined
var doubler_3 = function doubler_3(x) {
    return x * x;
};

// If you have no args you need to use () on LHS of =>
// let logger = => console.log("hi") // syntax error
var logger = function logger() {
    return console.log('hi');
};

// If you have more than 1 arg you must wrap args in parens
//let thing_1 = a,b => a + b // syntax error
var thing_1 = function thing_1(a, b) {
    return a + b;
}; // syntax error

// If you want to use the super short syntax and return an object,
//use () around the function body
// let thing_2 = x => {thing: x } // syntax error
var thing_2 = function thing_2(x) {
    return { thing: x };
};
```
