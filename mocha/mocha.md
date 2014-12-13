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


## Mocha and promises

Expectations throw an exception when they fail
    consequences:
        code after a failure will not be run!
Mocha wraps the test run in a try-catch and converts the exception to a test failure
but promises also absorb excpetions
    you can get around this by passing the error into the promise chain's `.done()` and calling Mocha's `done(error)` from there

```js
it('should work', function(done) {
    promise.then(function(value) {
        // success path
        expect(value).to.equal('blah');
        done();
    })
    .done(null, function(error) {
        // failure path
        done(error);
    });
    // .done(null, done) // shorter alternative to the above
});

The
* mocha-as-promised https://www.npmjs.org/package/mocha-as-promised
    * just return a promise from the `it` call - if it fullfills the test passes, fails then the test fails
* chai-as-promised https://www.npmjs.org/package/chai-as-promised
    * adds a bunch of `expect(foo).to.eventually.equal(bar)` style matchers
libs make this syntax nicer
