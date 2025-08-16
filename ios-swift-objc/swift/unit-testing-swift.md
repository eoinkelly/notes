XCode automatically includess `XCTest.framework` into your project

- Unit tests
- UI tests
    - can record user interactions to make a test
    - only available on OS X 10.11+ and iOS9+

All test cases are subclasses of `XCTestCase Testing frameworks

- Apple built-in XCTest
- https://github.com/Quick/Quick (rspec like)

Assertion libraries

- Built-in XCAssert
- https://github.com/Quick/Nimble (rspec like)

## XCTest

- A test method is an instance method of a test class that begins with the
  prefix test, takes no parameters, and returns void
- the `setup()` and `teardown()` methods are run before and after each test -
  they are "before each" not "before all"
- by default tests are executed synchronously

Asynchronous tests

- you can wait for a callback to fire before finishing the test
- TODO

Performance tests

- takes a block of code you want to profile
- runs it 10 times
- collects average run time and stddev of the run times

- allows you to set a "baseline" result for average runtime and fail the test if
  that baseline is exceeded
    - baselines are stored per device configuration
    - baselines are set in the GUI not in code

Swift

- Allowing testing of swift internals:
    - swift makes its internals available for testing when the `-enable-testing`
      flag is passed (Xcode has a build setting which does this automatically
      for test targets)o
- Allowing testing of your app's internals
    - `@testable import MyAppName` in the test file will make all "internal"
      entities available.
    - note: "private" entities are not available to test!

XCAssert

<assertion func name>(<expression that evals to a boolean>,
<error message string>)

XCTFail(message) XCTAssertEqual(ob1, ob2, message)
