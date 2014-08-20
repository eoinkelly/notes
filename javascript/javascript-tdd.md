Inbox
=====

* http://msdn.microsoft.com/en-us/magazine//ff452703.aspx

Questions
    what are defns of mocks, spies, stubs

there is the notion of testing state and testing behaviour. traditional unit tests are testing state but in the front end we often want to test behaviour

Javascript Testing Frameworks
-----------------------------
* http://sinonjs.org/
    * written by guy who wrote TDD javascript book
* http://pivotal.github.com/jasmine/
    * a BDD framework
* http://visionmedia.github.com/mocha/
    * does not come with an assertion lib builtin
    * http://chaijs.com/ - a BDD/TDD assertion library, not a test framework
    * also supports a browser post of node assert module as an assertion lib https://github.com/jxck/assert
* http://docs.jquery.com/QUnit
    * pavlov adds BDD style methods

* raw asserts:
    * is a assertion library module packaged with node
    * a
* http://www.jsunit.net/
* http://code.google.com/p/js-test-driver/
* Selenium
    *  test the client application as a simulated user through the browser.

Types of Tests
--------------

* Smoke tests
* Regression tests
    * jasmine is suitable
* Unit tests
    * jasmine is suitable
    * a unit test is a program (known as a test case, or test specification) that isolates and tests a specific and small functional unit of source code.
    * Never test too many pieces of code at one time.
    * And, unlike with an integration test, do not use unit tests to test more than one system at a time.
* Integration tests
    * they are characterised by testing more than one system at a time
    * jasmine isn't great at this but can do it a bit
* User acceptance tests
    Acceptance testing is done, ideally, by business or end users.

refactoring is *really* hard/impossible unless you have separations in your code somewhere - interfaces that do not change and hide internal functionality so that you can refactor that chunk and still behave the same way from an outsiders perspective - you need a notion of "inside" and "outside" - tests behave like outsiders and just call the interface.

testing first is the best way to make sure you code is testable!

To be able to test a piece of code you need to be able to call it by name so you can't test anonymous functions?

Writing Testable Code Talk (http://www.youtube.com/watch?v=XcT4yYu_TTs&feature=share&list=PL20115B23208B1033)
--------------------------------------------------------------------------------

Nobody ever says they don't know how to write a test. but writing tests is a skill you have to learn

singletons are global state - they create hard to test code

"new operators make it *impossible* to test code"

writing tests is a function of the *structure* of the code, not a function of *what* the code does.
when presented w. code that you have to test, don't try to understand what the code does. Try instead to understand the structure of the code

Red flags that indicate testable code.
* Global state aka singleton
* Law of Demeter violiation
* Global refernce to time
* hard-coded 'new' Operator
* lack of Dependency Injection

In Java the 'static' keyword implies a singleton implies global state