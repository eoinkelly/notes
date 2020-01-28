
# Comma operator

```js
(0, foo)();
// vs
foo();
```

* The comma operator evaluates both of its operands (from left to right) and returns the value of the second operand.
* the expression `(0, foo)` returns a _value_ whereas `foo` is an _lvalue_
    how is that relevant?
* this trick is sometimes used with `eval` to make sure it evaluates in global scope
    ```js
    (0, eval)(); // indirect call to eval, executes in global scope
    // vs
    eval(); // direct call to eval, executes in current scope
    ```
* babel uses it when transpiling ES6 modules to commonjs
