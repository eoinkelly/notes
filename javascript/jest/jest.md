```bash
# Install
npm i -D jest
npm i -D babel-jest @babel/core @babel/preset-env

# required for the babel command to see compiled output
npm i -D @babel/cli

# Now create babel.config.js by copy pasta the file below

# compiles file and displays on stdout for debugging
npx babel foo.test.js |bat -l js

# compile the es6 module file and make it readable
npx babel foo.test.js | npx prettier --stdin --parser babel > foo.test.compiled.js
```

```js
// babel.config.js
module.exports = {
    presets: [
        [
            '@babel/preset-env',
            {
                targets: {
                    node: 'current'
                }
            }
        ]
    ]
};
```

- Jest seems to be written at least partly in typescript
- As of 2020-01-01 jest test files are cjs so you cannot use `import` at the top
  of test files
    - jest implements their own version of node `require()`
        - they do this to allow easy mocking. Doing the same for modules would
          be a huge amount of work.
        - this is a really key point about jest. Jest only knows how to mock
          commonjs modules so you **must** transpile to cjs before jest can run
          your code
    - work on changing this is blocked until node ships a stable module loader
      API
    - you can work around this by
        1. use babel to compile your mjs to cjs
    - typescript gets transpiled anyway so using typescript doesn't have this
      issue
- As of 2020-01-01 all your tests run as commonjs - either babel or typescript
  compiles your ES6 modules into commonjs equivalents
    - so in a sense, commonjs is the "real" low-level implementation and ES6
      modules are (still) just sugar in jest
    - => I can't think of cjs and mjs as being "different" in Jest - better to
      think of mjs as sugar for cjs - albeit sugar with quite different
      semantics and sugar which will eventually become the real thing
- mjs and cjs have different semantics (mjs has live bindings, cjs does copies
  of all data) but it all works ok **provided your transpiler is compling both
  the test file and the imported file**
    - if you are pulling in a module that isn't being transpiled then these
      issues could bite I think
    - => if you transpile for your tests you must also transpile for production!

The `jest` object

- in scope in every test file

> Modules that are mocked with jest.mock are mocked only for the file that calls
> jest.mock. Another file that imports the module will get the original
> implementation even if it runs after the test file that mocks the module.

> Note: By default, jest.spyOn also calls the spied method. This is different
> behavior from most other test libraries
