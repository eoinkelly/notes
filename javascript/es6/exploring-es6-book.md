# Exploring es6

* <http://exploringjs.com/es6/>

About

* the **bodies** of es6 classes and modules are implicitly in `use strict` mode
    * => strict mode is defacto standard in es6
    * => you still need to add "use strict;" to files if you are not using modules!
* in his terminology "protocol" = interface + rules of use
    * => does not mean the same as protocol in objC or swift (there protocol = interface)
* he uses typescript annotations to describe function signatures

JS methods
`foo.bar()` foo is the receiver of the method call (he doesn't call it a message) and is available as `this` inside the method

JS internal properties

* part of the spec
* not accessible from JS
* denoted by `[[internalPropertyName]]`
* examples

```
[[Prototype]]       Object.getPrototypeOf(someObj)
```

JS Environment

* The environment is a data structre refered to by the spec to store variables for a particular scope
    * basically a dictionary of var_name -> var_value
* A "binding" is an entry in the environment dictionary

Destructrive operations

* Some JS functions modify their reciever e.g. `someArray.push(someValue)`
* Some JS functions return a modified copy of their receiver e.g. `someArray.concat(someValue)`

Ecmascript harmony process

1. Sketch (strawman)
1. Offical proposal
1. Implemenations. Must be implemented in 2+ implementations
1. Standard. If feature is still deemed good after a period of time it is put in the standard


Named function parameters:
* There is no built-in support for named parameters.
* Instead, the existing practice of naming parameters via object literals is supported via destructuring in parameter definitions.

Both Typescript and Flow are "es6 + types"


## Recommend: move from var to let/const

var
* scoped to function
* hoisted to top of function (always exists in its scope no matter what line it was declared on
* will add property to `window` global scope if used outside function
* requires you to use an IIFE to setup a new scope
* IIFEs are deprecated in ES6!

let

* block scoped
* let never creates properties of the global object, not even when used in global scope.
* => you can use curly-brace delimited blocks to setup a new "scope"

const

* `let` but will throw error if you try to change its value
* note it does not make compound values immutable e.g. if it points at an array you can still add/remove things but you canot assign it to a different array

AR recommends using `const` where possible, then `let` if needed and to avoid `var`

## Aside: how to achieve immutable objects in JS

const helps but is not enough

not sure what the diff between these is:
Object.freeze()
https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects/Object/freeze
Object.seal()
https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects/Object/seal

## :: bind operator - find out more

## a new kind of string: template literal

* a new kind of string literal (they are careful to call it them "template literals" not "template strings")
* two kinds
    * template literal
    * tagged template literal is a **function call** whose params are provided by a template literal
        * note that "tagged template literal" is a kind of function call not a kind of string literal

* allows multiline strings
* allows variable interpolation

```js
let ex1 = `I am
a mul
```

Tagged template literal

* allows you to use JS as a parser
* you give it a string containing your DSL and the function you write gets a "quasi tokenized" version of the string
