
Type narrowing happens in JS - TS is just good at following along.

## The tools of type narrowing in JS

1. truthy/falsy e.g. `if (thing) { ...`
    * Seven common falsy values in JS (8 if you count bigint
        ```js
        false       // 1
        ""          // 2
        0           // 3
        -0          // 4
        null        // 5
        undefined   // 6
        NaN         // 7
        //bigint 0, 0n == 0 (loose comparison) but 0n !== 0 (strict comparison)
        0n          // 8

        // falsy because it was historically used to detect old browsers - see
        // https://stackoverflow.com/questions/10350142/why-is-document-all-falsy
        document.all
        ```
    * ++ excludes both `null` and `undefined` in the same check
    * -- has edge cases if you are expecting numbers (0 is falsy) or booleans (false is falsy)

2. `object instanceof Constructor` operator
    * is an operator
    * tests if the prototype property of the RHS argument appears anywhere in the prototype chain of the LHS argument
    * can throw a `TypeError` if
      * constructor is not an object
      * constructor must either have a `@@hasInstance` method or constructor must be a function
      * you can break it e.g.
        ```js
        let array = []
        array.__proto__ = {}
        array istanceof Array // false
        ```
3. `typeof` operator
4. exclude specifically `null`
    * `val === null`
5. exclude specifically `undefined`
    * `val === undefined`
6. `Array.isArray`

##  instanceof operator

```js
object instanceof Constructor
```

* an operator
* tests if the prototype property of the RHS argument appears anywhere in the prototype chain of the LHS argument
* can throw a `TypeError` if
    * constructor is not an object
    * constructor must either have a `@@hasInstance` method or constructor must be a function
* Gotchas
  * string primitives are not instances of `String()`

```js
function C() {}

let o = new C()

o instanceof C // true
// Object.getPrototypeOf(o) === C.prototype
```




```