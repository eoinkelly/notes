## Terminology

* `describe` creates a _test suite_
* The top level _test suite_ has no name and no visible describe function but its existance allows you to run `beforeEach` and friends at the top level of your tests.
* omit the callback function to `it` to create a pending test

describe
describe.only
describe.skip
it
it.only
it.skip

A test fails when it throws and error so any assertion framework that throws an error will work

### Diffs

    err.acutal
    err.expected

are shown in the output if they exist.
Currently on string diffs are supported

supports many reporters from the command line but only the `HTML` reporter in the browser

`mocha.setup` is a shortcut for giving all config settings in the browser in one call

You can control timeout at suite or test level with `this.timeout(400)`
`this.timeout(0)` disables timeouts completely

mocha defaults to looking for tests in `./test/*.js` so that is a good default


