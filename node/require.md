# Node require function

- JS files and modules are 1:1 in node

module.exports

- a POJO that always exists in a module

- that each time you subsequently require an already-required file, the exports
  object is cached and reused.
- modules are cached based on their resolved filename so even modules with the
  same name in different locations should do the right thing.

```js
// each module exports exactly 1 object
// set the object manually
module.exports = foo;


// add a property 'blah' to the default exports object
exports blah = 12;

exports foo = 14;

// equivalent to ...
module.exports = {
    blah: 12,
    foo: 14
}
```

## require()

- Finds and evals the javascript file (aka module)
- node core moduels are compiled into the binary
    - always preferentially loaded over files with the same names
- Each node module (a single js file) is evaluated _only once_ - the result of
  evaluation is cached for subsequent uses.

- require finds files by:
    - if the file doesn't start with "./" or "/", then it is either
        1. a core module (and the local Node path is checked),
        2. a dependency in the local node_modules folder. The search for
           `require('bar');` will start in `./node_modules`
    - If the file starts with "./" it is considered a relative file to the file
      that called require.
    - If the file starts with "/", it is considered an absolute path. NOTE: you
      can omit ".js" and require will automatically append it if needed.
    - if the filename passed to require is actually a directory, it will first
      look for package.json in the directory and load the file referenced in the
      main property. Otherwise, it will look for an index.js.
    - NOTE: `package.json` can be involved in how `require` works

http://thenodeway.io/posts/how-require-actually-works/

- require loads the file into an instance of `Module`
- within the file that instance is available as `module`
- `module` is not a real global - you get a different instance of it within each
  module
- it has the properties
    - `parent:`
    - `id:`
    - `exports: {}`

### exports

- exports starts as an alias to `module.exports`
    ```js
    var exports = module.exports;
    ```
- if you overwrite it it will not only change `exports` not `module.exports`

```js
// this is what your module code ends up being wrapped in before being executed
// by Module._compile()
(function (exports, require, module, __filename, __dirname) {
    // YOUR CODE INJECTED HERE!
});
```
