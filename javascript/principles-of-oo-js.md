# Chapter 1: Javascript types

## 5 primitive types

1. string
2. number
3. boolean
4. undefined
    * `undefined` is both the name of the type and the single legal value of the type
5. null
    * `null` is both the name of the type and the single legal value of the type

To allow you to call methods on primitives JS will wrap them in a corresponding
object type for just that call. In general avoid using String, Number, Boolean
directly. Boolean in particular is bad:

```js
var x = new Boolean(false)
if (x) { console.log('uh oh'); }
uh oh
```

## 6 built-in reference types

Variables of these types are basically pointers to the heap

1. Object
2. Array
3. Date
4. Error
5. Function
6. Regexp

constructor | literal syntax
----------- | ---------------
Object      | {}
Array       | []
Date        |
Error       |
Function    | `function foo() { ... }` and `var x = function() { ... }`
Regexp      | /some|regex/


Literal syntaxes do not just call the constructor so you shouldn't override
those built-in constructors:

```js
var x = {}; // does not actually call new Object() so overrides will not be seen
```

```js
// 1. string
typeof "thing" === "string"

// 2. number
typeof 34 === "number"

// 3. boolean
typeof true === "boolean"

// 4. undefined
typeof undefined === "undefined"

// 5. null
someval === null
// typeof null returns "object"

// 6. Object
x instanceof Object // but will also match Function, String, Number, Array
_.isPlainObject(x) // lodash way
// https://github.com/lodash/lodash/blob/2.4.1/dist/lodash.compat.js#L2857
// seems complex enough to want to use the lodash way

// 7. Array
somearry instanceof Array // works in ES3, not across iframes
Array.isArray(somearry) === true // works for arrays passed across between iframes too, requires ES5 (no IE8 or older)

// 8. Date
x instanceof Date

// 9. Error
x instanceof Error

// 10. Function
typeof somefunc === "function"
somefunc instanceof Function

// 11. Regexp
x instanceof RegExp

```

## Chapter 2 - Functions

* JS has the idea of _internal properties_ which are not accessible to the
  developer and are denoted with `[[InternalPropName]]` in docs and the spec.
* Function objects have a `[[Call]]` internal property which contains the
  execution instructions for the function
* Functions **are** objects!
* they have a `length` property which is the number of args teh function expects
* There are 3 ways to control `this` within a function
    1. func.call(thisval, arg1, arg2 ...)
    2. func.apply(thisval, argsArray)
    3. func.bind(thisval, arg1) // returns a new function with bound this and first arg

Other languages have _function signatures_ which contain

1. function name
2. number of args
3. type of each arg

A function signature uniquely identifies a function and the language can tell
two functions apart if any part of their signature is different e.g.

* different number of args
* same number of args but with different types

Javascript has no function signatures so you cannot have variations of the same
function (i.e. function overloading)

Functions have 2 literal forms


```js
// function _declaration_
function foo() {
}

// function expression assigned to a variable
var foo = function() {
}
```

Key differences between them

* Function declaration
    * hoisted
* Function expression
    * not hoisted!
        * cannot be hoisted because the funciton is refered to as the value of
          a variable (the _declaration_ of the variable you assign it to _is_
          hoisted)
